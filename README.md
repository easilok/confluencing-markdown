# confluencing-markdown

A cli application to convert a Confluence document into a simplified markdown file.

## Requirements

This application is built on top of `guile 3` and requires the
[guile-json](https://github.com/aconchillo/guile-json) module for parsing the JSON output from Atlassian's REST API.

## Usage

The entrypoint of the application is located at the `src/main.scm` which is itself an executable script file that can be
run with `./src/main.scm`.

> [!IMPORTANT]
> If the script fails to execute properly, run as a guile script with the command 
> `guile -L src -e main -s src/main.scm`.

The command full usage signature:

```shell
./src/main.scm [--output/-o --logger/-l --log-file] input.json
```

### Command line arguments

- `--ouput`/`-o` sets the file path to save the markdown version of the received confluence page. It's optional and, if
  note provided, will use standard output to print the markdown content.
- `--logger/-l` activates the application logger. If no `log-file` is provided, it will use the `.log` as file.
- `--log-file` sets a custom file path to use as the application logger. If provided, the `logger` argument can be
  omitted.
- `input.json` is the only required argument and contains the confluence page API response for parsing.

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
- [ ] **Numbered list**, what is the block type?
- [ ] **Nested list**, add parsing of nested list.
- [ ] **Images**, they are only provided as an id field. How can they be used here?
- [ ] **Caption**, which is a content object of the `mediaSingle` block. How to handle it with proper format?
- [ ] Fetch from confluence by space and page id:
    - Use env/config for setting token (and CLI flag to override)
    - CLI flags for space (default in config) and page ID
    - CLI flag to output name (default to space+id/fetch name)
