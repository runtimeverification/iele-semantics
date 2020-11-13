const fs = require("fs");
const path = require("path");
const MarkdownIt = require("markdown-it");
const glob = require("glob");
const cheerio = require("cheerio");
const hljs = require("highlight.js"); // https://highlightjs.org/
const k = require("./highlight.js/k");
const G = require("glob");
const url = require("url");
hljs.registerLanguage("k", k);

const md = new MarkdownIt({
  html: true,
  linkify: true,
  highlight: function (str, lang) {
    lang = lang.replace(/^\{\./, "").replace(/\}$/, "").trim();
    if (lang && hljs.getLanguage(lang)) {
      try {
        return (
          '<pre class="hljs"><code>' +
          hljs.highlight(lang, str, true).value +
          "</code></pre>"
        );
      } catch (__) {}
    }

    return (
      '<pre class="hljs"><code>' + md.utils.escapeHtml(str) + "</code></pre>"
    );
  },
});
md.use(require("markdown-it-anchor"));

const files = {
  "./static_content/html/404.html": "404.html",
  "./static_content/html/500.html": "500.html",
};

const outPath = "./public_content/";
const basePath = "static_content/html/";
const regexp = /{{(.*)}}/;

/**
 *
 * @param {string} sourceHTML the HTML content
 * @param {string} targetFilePath path relative to current __dirname
 * @param {object} variables variables map
 */
function generateOutputWebpage(sourceHTML, targetFilePath, variables = {}) {
  const filePath = targetFilePath.startsWith("/")
    ? targetFilePath
    : path.join(__dirname, outPath, targetFilePath);
  const dirname = path.dirname(filePath);

  if (!fs.existsSync(dirname)) fs.mkdirSync(dirname, { recursive: true });

  const relative = path.relative(dirname, outPath);

  const resultHTML = sourceHTML
    .split("\n")
    .map((line) => {
      const match = line.match(regexp);
      let content = line;

      if (match && match.length == 2 && !match[1].startsWith("$")) {
        content = fs.readFileSync(basePath + match[1]).toString();
      }

      // Fix assets folder path error for github page
      content = content.replace(/{{\$(.+?)}}/g, (_, variableName) => {
        if (variableName === "ROOT") {
          return relative || ".";
        } else if (variableName in variables) {
          return variables[variableName];
        } else {
          return _;
        }
      });

      return content;
    })
    .join("\n");
  fs.writeFileSync(filePath, resultHTML);
  console.log("Written file: " + filePath);
}

/**
 * @param {string} globPattern
 * @param {G.IOptions} globOptions
 * @param {string} sourceDirectory
 * @param {string} outputDirectory
 * @param {string} template
 */
function generatePagesFromMarkdownFiles({
  globPattern,
  globOptions = {},
  origin = "",
  sourceDirectory,
  outputDirectory,
  template = "",
}) {
  const files = glob.sync(globPattern, globOptions);
  for (let i = 0; i < files.length; i++) {
    const file = files[i];
    const targetFilePath = path.resolve(
      path.resolve(
        outputDirectory,
        path.relative(sourceDirectory, path.dirname(file))
      ),
      path.basename(file).match(/^(index|README)\.md$/i)
        ? "index.html"
        : `${path.basename(file).replace(/\.md$/, "")}/index.html`
    );
    let markdown = fs.readFileSync(file).toString("utf-8");
    if (
      markdown.startsWith("---") &&
      /* tslint:disable-next-line:no-conditional-assignment */
      (endFrontMatterOffset = markdown.indexOf("\n---")) > 0
    ) {
      markdown = markdown
        .slice(endFrontMatterOffset + 4)
        .replace(/^[ \t]*\n/, "");
    }
    const html = md.render(markdown);

    // Format links
    const $ = cheerio.load(html);
    $("a").each((index, anchorElement) => {
      try {
        let href = $(anchorElement).attr("href");
        if (href.match(/^(https?|mailto):/)) {
          $(anchorElement).attr("target", "_blank");
          $(anchorElement).attr("rel", "noopener");
        } else if (href.match(/\.md(#|$)/)) {
          // might be ./README.md or ./README.md#tag
          if (
            href.startsWith("../") &&
            !href.match(/(index|README)\.md(#|$)/)
          ) {
            href = "../" + href;
          }
          $(anchorElement).attr(
            "href",
            href.match(/(index|README)\.md(#|$)/)
              ? href.replace(/(index|README)\.md/, "")
              : href.replace(/(?:\/|^)(.+?)\.md/, ($0, name) => {
                  if (path.basename(file).match(/^(index|README)\.md$/i)) {
                    return `./${name}/`;
                  } else {
                    return `../${name}/`;
                  }
                })
          );
        } else {
          $(anchorElement).attr("href", url.resolve(origin, href));
        }
      } catch (error) {}
    });

    generateOutputWebpage(template, targetFilePath, {
      TITLE: targetFilePath,
      MARKDOWN_HTML: $.html(),
    });
  }
}

function cleanUpFiles() {
  const kDistributionPath = path.join(
    __dirname,
    "./public_content/k-distribution"
  );
  if (fs.existsSync(kDistributionPath)) {
    fs.rmdirSync(kDistributionPath, {
      recursive: true,
    });
  }

  const files = glob.sync(
    path.join(__dirname, "./public_content/") + "/**/*.html"
  );
  files.forEach((file) => {
    fs.unlinkSync(file);
    const dirPath = path.dirname(file);
    const filesInside = fs.readdirSync(dirPath);
    if (!filesInside.length) {
      fs.rmdirSync(dirPath, { recursive: true });
    }
  });
}

for (file in files) {
  generateOutputWebpage(fs.readFileSync(file).toString("utf-8"), files[file]);
}

cleanUpFiles();
const pageTemplate = fs
  .readFileSync("./static_content/html/page_template.html")
  .toString("utf-8");
//generatePagesFromMarkdownFiles(
//  path.resolve(__dirname, "./pages/") + "/**/*.md",
//  path.resolve(__dirname, "./pages/"),
//  path.resolve(__dirname, "./public_content/"),
//  pageTemplate
//);
generatePagesFromMarkdownFiles({
  globPattern: path.resolve(__dirname, "../") + "/**/*.md",
  globOptions: { ignore: [path.resolve(__dirname, "../web/**/*")] },
  origin: "https://github.com/runtimeverification/iele-semantics/tree/master/",
  sourceDirectory: path.resolve(__dirname, "../"),
  outputDirectory: path.resolve(__dirname, "./public_content/"),
  template: pageTemplate,
});
