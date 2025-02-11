# confluencing-markdown

A cli application to convert a Confluence document into a simplified markdown file.

## Requirements

This application is built on top of `guile 3` and requires the
[guile-json](https://github.com/aconchillo/guile-json) module for parsing the JSON output from Atlassian's REST API.

## TODO

- [ ] **Text block marks**, which sets the text styles, like bold, code, etc. Style is present at `"marks"[]."type"`.
- [ ] **Numbered list**, what is the block type?
- [ ] **Nested list**, add parsing of nested list.
- [ ] **Table list**, add conversion of table cell list to semicolon separated values.
- [ ] **Newline cleanup**, several blocks add two newlines for context spliting, making multiple newlines possible.
  Final result need to cleaned from more than two newlines into two newlines.
- [ ] **Paragraph**, should this block impose a newline?
- [x] **Mention**, which has the mention name at `"attrs"."text"`.
- [x] **Panel**, which has `"attrs"."panelType": "warning"` for setting its type.
- [x] **Expand**, which has `"attrs"."title"` as the expandable block title.
- [ ] **Caption**, which is a content object of the `mediaSingle` block. How to handle it with proper format?
- [ ] Split code in modules.
- [x] logging system
- [ ] Output parsed content to file.
- [ ] Fetch from confluence by space and page id:
    - Use env/config for setting token (and CLI flag to override)
    - CLI flags for space (default in config) and page ID
    - CLI flag to output name (default to space+id/fetch name)
