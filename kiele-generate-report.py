#!/usr/bin/env python3

from dataclasses import dataclass, asdict
import json
from typing import Dict, List, Optional, Tuple
from textwrap import wrap
import uuid
import os
from dacite.core import from_dict
from shutil import copyfile, rmtree, make_archive
from urllib.parse import quote
import sys
from datetime import datetime


# All class below are translated from the Haskell code at
# the PR: https://github.com/runtimeverification/firefly-web/pull/306/

@dataclass
class Report:
    status: str
    tag: Optional[str]
    hasFireflyLog: bool
    created: str
    reportId: str
    token: str
    coverage: str
    commit: Optional[str]
    type: str


@dataclass
class IeleContract:
    name: str
    sourceMap: str
    coverage: str
    asm: Optional[str] # iele code


"""
key is the contract hash
"""
IeleContracts = Dict[str, IeleContract]


@dataclass
class IeleReport:
    contents: str # Iele code if source file name ends with `.iele`. otherwise it's solidity code whose source file name should end with `.sol`
    contracts: IeleContracts


"""
key is the source file name
"""
IeleReports = Dict[str, IeleReport]


@dataclass
class Source:
    fileId: int
    filename: str
    sourceLines: List[str]
    asmSourceLines: Optional[List[str]]


@dataclass
class CoveredState:
    tag: str
    contents: Optional[List[int]]  # (covered, total)


Coverages = List[Tuple[int, List[Tuple[int, CoveredState]]]]


@dataclass
class CoverageMap:
    """
    Solidity files source code
    """
    ieleSources: List[Source]
    """
    Solidity coverage data
    """
    ieleCoverage: Coverages  # Dict[int, Dict[int, CoveredState]]


@dataclass
class ContractArtifact:
    contractName: str
    sourceName: Optional[str]
    coverageMap: CoverageMap
    coverage: int
    fileId: int


@dataclass
class CoverageSummary:
    bytecodeHash: str
    account: Optional[str]
    contractName: Optional[str]
    sourceName: Optional[str]
    """
    Coverage percent
    """
    coverage: int
    """
    Id of source file the contract was defined in
    """
    mainContract: Optional[int]

def line_from_pos(offset: int, text: str) -> int:
    """
    Given a text and an offset into the text, get the line number
    """
    return len(text[0:offset+1].splitlines()) - 1

def make_coverage_summaries(artifacts: List[ContractArtifact]) -> List[CoverageSummary]:
    summaries: List[CoverageSummary] = []
    for artifact in artifacts:
        summaries.append(CoverageSummary(bytecodeHash=artifact.contractName, account=None, contractName=artifact.contractName,
                                         sourceName=artifact.sourceName, coverage=artifact.coverage, mainContract=artifact.fileId))
    return summaries


def make_coverage_map(source_name: str, content: str, asm: Optional[str], file_id: int, source_map: str, coverage: str) -> Tuple[CoverageMap, int]:
    lines: List[int] = []
    prev_line = -1
    for source_map_entry_str in source_map.split(";"):
        line_str = source_map_entry_str.split(":")[0]
        if line_str.strip() == "":
            lines.append(prev_line)
        else:
            prev_line = (int(line_str) - 1) if source_name.endswith(".iele") else (line_from_pos(int(line_str), asm or "") - 1)
            lines.append(prev_line)

    states = get_states(coverage)
    coverage_ = calculate_coverage(states)
    coverage_map: CoverageMap = CoverageMap(
        ieleSources=[Source(
            fileId=file_id, filename=source_name, sourceLines=content.splitlines(), asmSourceLines=asm.splitlines() if asm != None else None)],
        ieleCoverage=[(file_id, list(zip(lines, states)))]
    )
    return (coverage_map, coverage_)


def get_states(coverage: str) -> List[CoveredState]:
    chunks: List[str] = wrap(coverage.replace("0x", "", 1), 2)
    states: List[CoveredState] = []
    for chunk in chunks:
        if chunk == "00":
            states.append(CoveredState(tag="NotCovered", contents=None))
        elif chunk == "01" or chunk == "0D":
            states.append(CoveredState(tag="Covered", contents=None))
        else:
            states.append(CoveredState(tag="Weak", contents=[0, 0]))
    return states


def calculate_coverage(states: List[CoveredState]) -> int:
    covered = 0
    for state in states:
        if state.tag == "Covered" or state.tag == "Weak":
            covered += 1
    return int(covered * 100 / len(states))


def convert_iele_reports_to_contract_artifacts(iele_reports: IeleReports) -> List[ContractArtifact]:
    artifacts: List[ContractArtifact] = []
    file_id = 0
    for source_name in iele_reports:
        file_id += 1
        iele_report = iele_reports[source_name]
        contents = iele_report.contents
        contracts = iele_report.contracts
        for hash in contracts:
            contract = contracts[hash]
            (coverage_map, coverage) = make_coverage_map(source_name,
                                                         contents, contract.asm, file_id, contract.sourceMap, contract.coverage)
            artifacts.append(ContractArtifact(contractName=contract.name, sourceName=source_name,
                                              coverageMap=coverage_map, coverage=coverage, fileId=file_id))
    return artifacts


def write_json_file(file_path: str, json: str):
    with open(file_path, "w") as f:
        f.write(json)

def generate_static_report(report_template_path: str, reports_json_path: str, output_report_path: str = "", create_report_archive: bool = False):
    report_id = str(uuid.uuid4())
    os.makedirs("./reports", exist_ok=True)

    with open(reports_json_path, "r") as f:
        j = json.loads(f.read())

    iele_reports: IeleReports = {}
    for source_name in j:
        iele_reports[source_name] = from_dict(
            data_class=IeleReport, data=j[source_name])
    artifacts = convert_iele_reports_to_contract_artifacts(iele_reports)
    if len(artifacts) == 0:
        raise Exception("no artifacts found")
    report_base_path = "./reports/" + report_id
    os.makedirs(report_base_path, exist_ok=False)

    # Write the summary file
    summaries = make_coverage_summaries(artifacts)
    with open(os.path.join(report_base_path, "./summary.json"), "w") as f:
        f.write(json.dumps(list(map(lambda summary: asdict(summary), summaries))))

    # Write coverage information files
    for artifact in artifacts:
        hash = artifact.contractName
        source_name = artifact.sourceName
        coverage_map = artifact.coverageMap
        write_json_file(os.path.join(report_base_path, quote(
            source_name + "-" + hash + "-iele.json", safe="")), json.dumps(asdict(coverage_map)))

    # Copy original
    original_path = os.path.join(report_base_path, "./original")
    os.makedirs(original_path)
    copyfile(reports_json_path,
             os.path.join(original_path, "./reports.json"))

    firefly_log_exists = os.path.exists("./firefly.log")
    if firefly_log_exists:
        copyfile("./firefly.log", os.path.join(original_path, "./firefly.log"))

    if not create_report_archive:
        # Create ${report_id}.json file
        report: Report = Report(status="success", tag=None, hasFireflyLog=firefly_log_exists, created=datetime.today(
        ).strftime('%Y-%m-%dT%H:%M:%SZ'), reportId=report_id, token="(generated)", coverage="ParseSuccess", commit=None, type="iele")
        write_json_file(os.path.join(report_base_path, report_id +
                                    ".json"), json.dumps(asdict(report)))

        # Create JavaScript code
        js_code = "window.FIREFLY_REPORT_FILES = {}"
        onlyfiles = [ f for f in os.listdir(report_base_path) 
                            if os.path.isfile(os.path.join(report_base_path, f))
                    ] + [ os.path.join("original", f) for f in os.listdir(os.path.join(report_base_path, "./original"))
                                                        if not f.endswith(".zip") ]
        for f in onlyfiles:
            with open(os.path.join(report_base_path, f), "r") as file:
                content = file.read()
            if not f.endswith(".json"):
                content = json.dumps(content)
            js_code += "\nwindow.FIREFLY_REPORT_FILES[\"" + \
                report_id + "/" + f + "\"] = " + content + "\n"

        # Inject the js code into the HTML report template file.
        with open(report_template_path, "r") as f:
            template = f.read()
        report_html = template.replace(
            "<body>", "<script>" + js_code + "</script>\n<body>")

        if output_report_path.strip() == "":
            output_report_path = report_id + ".html"

        with open(output_report_path, "w") as f:
            f.write(report_html)
    else:
        # make report.zip file
        if os.path.exists("report.zip"):
            os.remove("report.zip")
        make_archive("report", "zip", report_base_path)
        
    # Clean up the report directory
    rmtree(report_base_path)
    if len(os.listdir("./reports")) == 0:
        rmtree("./reports")

    return output_report_path

report_template_path = sys.argv[1]
reports_json_path = sys.argv[2]
output_report_path = ""
if len(sys.argv) > 3:
    output_report_path = sys.argv[3]
create_report_archive = False
if len(sys.argv) > 4:
    create_report_archive = (sys.argv[4] == "true")
if not create_report_archive:
    print("Generating report")
output_report_path = generate_static_report(report_template_path, reports_json_path, output_report_path, create_report_archive)
if not create_report_archive:
    print(output_report_path + " generated")
