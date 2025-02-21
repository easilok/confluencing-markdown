# confluencing-markdown

A cli application to convert a Confluence document into a simplified markdown file.

## Requirements

This application is built on top of `guile 3` and requires the following extra libraries:

- [guile-json](https://github.com/aconchillo/guile-json) module for parsing the JSON output from Atlassian's REST API.
- [guile-gnutls](https://gitlab.com/gnutls/guile/) used by the webserver client for HTTPS requests.

## Usage

The entrypoint of the application is located at the `src/main.scm` which is itself an executable script file that can be
run with `./src/main.scm`.

> [!IMPORTANT]
> If the script fails to execute properly, run as a guile script with the command 
> `guile -L src -e main -s src/main.scm`.

The command full usage signature:

```shell
./src/main.scm [--output/-o --logger/-l --log-file --config/-c --base-url/-b --page-id/-p input.json]
```

### Command line arguments

- `--ouput`/`-o` sets the file path to save the markdown version of the received confluence page. It's optional and, if
  note provided, will use standard output to print the markdown content.
  - `--logger`/`-l` activates the application logger. If no `log-file` is provided, it will use the standard output as long
  as the `--output`/`-o` is provided.
- `--log-file` sets a custom file path to use as the application logger. If provided, the `logger` argument can be
  omitted.
- `--config/-c` sets the path to the application configuration file.
- `--base-url/-b` sets the Confluence API base URL, which is tight to the workspace in use. 
- `--page-id/-p` sets id of the page to fetch from Confluence API.
- `input.json` is the remaining unnamed argument and contains the path to a file containing a page API response for
  parsing.

**None of the presented CLI arguments are required**, but at least either `input.json` or properties to fetch data from
Confluence API are required in order to parse a document. The Confluence API properties can also be provided in the
configuration file.

> ![IMPORTANT]
> Any provided CLI argument takes priority over the configuration file, overriding it.

### Configuration file

The application also allows for configuring some of its used properties. The configuration is made in a `JSON` file and
provided to the application with the `--config/-c` command line option.

An example configuration file is available at [config.example.json](./config.example.json) with properties:

- **username**: the email with access to the Confluence API.
- **password**: the generated API token, from the provided email with access to the Confluence API.
- **base-url**: the base URL for the Confluence API of the workspace that owns the desired page.
- **page**: the Confluence page ID to fetch from the API.

## TODO

- [x] **Table list**, ~~add conversion of table cell list to semicolon separated values~~, trimmed start and ending new
  lines and converted interior new lines to `<br/>`.
- [x] **Mention**, which has the mention name at `"attrs"."text"`.
- [x] **Panel**, which has `"attrs"."panelType": "warning"` for setting its type.
- [x] **Expand**, which has `"attrs"."title"` as the expandable block title.
- [x] logging system
- [x] Split code in modules.
- [x] **Text block marks**, which sets the text styles, like bold, code, etc. Style is present at `"marks"[]."type"`.
- [x] **CLI Flags**, add flags to:
    - Output file (--output/-o)
    - Enable logger (--logger-file)
    - Input file (last non option arg)
- [x] **Paragraph**, new new lines to represent new paragraphs.
- [x] **Newline cleanup**, several blocks add two newlines for context spliting, making multiple newlines possible.
  Final result need to cleaned from more than two newlines into two newlines.
- [x] **Refactor tables**, as markdown tables require header + separator row, this format is incompatible with the
  header column table block of confluence. Tables require a refactor to:
    - Add the separator row when table has already an header row
    - Add an empty header row and separator if there is an header column
- [x] Fetch from confluence by space and page id:
    - Use env/config for setting token (and CLI flag to override)
    - CLI flags for space (default in config) and page ID
    - CLI flag to output name (default to space+id/fetch name)
- [ ] **Numbered list**, what is the block type?
- [ ] **Nested list**, add parsing of nested list.
- [ ] **Images**, they are only provided as an id field. How can they be used here?
- [ ] **Caption**, which is a content object of the `mediaSingle` block. How to handle it with proper format?
- [ ] **inlineCard**, ??
- [ ] **rule**, ??
- [ ] **blockquote**, ??
- [ ] **placeholder**, should transcribe as HTML comment if presence is desired.
