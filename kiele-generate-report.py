#!/usr/bin/env python

from dataclasses import dataclass, asdict
import json
from typing import Dict, List, Optional, Tuple
from textwrap import wrap
import uuid
import os
from dacite.core import from_dict
from shutil import copyfile, rmtree
from urllib.parse import quote
import sys


# All class below are translated from the Haskell code at
# the PR: https://github.com/runtimeverification/firefly-web/pull/306/


@dataclass
class IeleContract:
    name: str
    sourceMap: str
    coverage: str


"""
key is the contract hash
"""
IeleContracts = Dict[str, IeleContract]


@dataclass
class IeleReport:
    contents: str
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
    sources: List[Source]
    """
    Solidity coverage data
    """
    coverage: Coverages  # Dict[int, Dict[int, CoveredState]]


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
    """
    Type of the report: iele|evm
    """
    type: str


def make_coverage_summaries(artifacts: List[ContractArtifact]) -> List[CoverageSummary]:
    summaries: List[CoverageSummary] = []
    for artifact in artifacts:
        summaries.append(CoverageSummary(bytecodeHash=artifact.contractName, account=None, contractName=artifact.contractName,
                                         sourceName=artifact.sourceName, coverage=artifact.coverage, mainContract=artifact.fileId, type="iele"))
    return summaries


def make_coverage_map(source_name: str, content: str, file_id: int, source_map: str, coverage: str) -> Tuple[CoverageMap, int]:
    lines: List[int] = list(map(lambda s: int(
        s.split(":")[0]) - 1, source_map.split(";")))
    states = get_states(coverage)
    coverage_ = calculate_coverage(states)
    coverage_map: CoverageMap = CoverageMap(
        sources=[Source(
            fileId=file_id, filename=source_name, sourceLines=content.splitlines())],
        coverage=[(file_id, list(zip(lines, states)))]
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
                                                         contents, file_id, contract.sourceMap, contract.coverage)
            artifacts.append(ContractArtifact(contractName=contract.name, sourceName=source_name,
                                              coverageMap=coverage_map, coverage=coverage, fileId=file_id))
    return artifacts


def generate_static_report(report_template_path: str, reports_json_path: str):
    report_id = str(uuid.uuid4())
    os.makedirs("./reports", exist_ok=True)

    # TODO: Maybe allow the this path to be passed as argument?
    f = open(reports_json_path, "r")
    j = json.loads(f.read())
    f.close()

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
    f = open(os.path.join(report_base_path, "./summary.json"), "w")
    f.write(json.dumps(list(map(lambda summary: asdict(summary), summaries))))
    f.close()

    # Write coverage information files
    for artifact in artifacts:
        hash = artifact.contractName
        source_name = artifact.sourceName
        coverage_map = artifact.coverageMap
        f = open(os.path.join(report_base_path, quote(source_name + "-" + hash + "-iele.json", safe="")), "w")
        f.write(json.dumps(asdict(coverage_map)))
        f.close()

    # Copy original
    original_path = os.path.join(report_base_path, "./original")
    os.makedirs(original_path)
    copyfile(reports_json_path,
             os.path.join(original_path, "./reports.json"))

    # Inject the js code into the HTML report template file.
    f = open(report_template_path, "r")
    template = f.read()
    f.close()
    report_html = template.replace(
        "<body>", "<script>" + "TODO: js code here" + "</script>\n<body>")
    f = open(report_id + ".html", "w")
    f.write(report_html)
    f.close()

    # Clean up the report directory
    rmtree(report_base_path)
    if len(os.listdir("./reports")) == 0:
        rmtree("./reports")

    return report_id + ".html"

report_template_path = sys.argv[1]
reports_json_path = sys.argv[2]
generate_static_report(report_template_path, reports_json_path)