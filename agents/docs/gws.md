# Google via `gws`

How any project talks to Drive, Docs and Sheets. Project skills add their own ids and
registry rules and point here for the rest.

## Identity

- Auth: `gws auth status`; if not logged in, stop and let the human log in. Tokens and
  client secrets never land in a repo.
- Shared Drive files: always `supportsAllDrives: true` (plus `driveId` on list/search).
- Scopes: `drive`, `docs`, `sheets` only, unless a skill names another.

## Calling `gws`

- Flags: `--params` = URL/query parameters (`spreadsheetId`, `documentId`, `fileId`,
  `supportsAllDrives`), `--json` = request body, `--upload <file>` = media. `--json` takes a
  string, so `--json "$(cat req.json)"`.
- Build request JSON with a script, never shell interpolation: an embedded `\n` becomes a
  400 "control character found while parsing a string".
- `gws` prints `Using keyring backend` on stderr; `2>/dev/null` when piping JSON.
- `--upload` accepts only paths under the current directory; `cd` to the file's directory first.
- `--dry-run` validates locally only (prints method and URL). It catches neither API
  rejections nor no-ops; the check is a re-fetch.

## Docs

- Read structure: `gws docs documents get --params '{"documentId":"<id>"}' > get.json`. A get
  is ~1 MB: always to a file, walked by a script. Edit: `gws docs documents batchUpdate`.
- **Fresh get before every batchUpdate.** Indices come from that get, never from memory;
  colleagues edit concurrently. Apply index-based requests highest-index first; put
  `replaceAllText` (index-free) after them.
- Pass `writeControl: {requiredRevisionId}` from the same get. A 400 = someone edited in
  between: re-read, redo.
- Paragraph style lives on the paragraph's trailing newline: deleting a full
  `startIndex..endIndex` range removes that paragraph and leaves the next one's style intact.
- Links to headings: `<url>#heading=<headingId>`; the id is on the paragraph in the get.
  A `headingId` is minted only on a style *transition*: setting the same `namedStyleType`
  again is a no-op. To mint one, send two `updateParagraphStyle` on the same range in one
  batch: `NORMAL_TEXT`, then the `HEADING_n` (`fields: "namedStyleType"`). The toggle also
  drops inline overrides on the paragraph.
- Links into a Drive PDF page: `https://drive.google.com/file/d/<id>/view#p=<n>` (1-based,
  hash, lowercase `p`); plain hyperlink, not a chip (a chip shows the title, loses the page).
- Rich links to Drive files: `insertRichLink` with `richLinkProperties.uri`.
- Import a `.docx` as native Doc: `drive files create --json '{"name":…,"mimeType":
  "application/vnd.google-apps.document","parents":[…]}' --params '{"supportsAllDrives":true}'
  --upload f.docx`. A plain upload leaves a `.docx` shell the Docs API cannot edit. The reply
  is a bare `id`; the URL is `https://docs.google.com/document/d/<id>/edit`. Plain-text
  export for review: `drive files export --params '{"fileId":…,"mimeType":"text/plain"}'`.

## Sheets

- Read: `gws sheets +read --spreadsheet <id> --range 'tab!A:H'`. This flattens a chip to
  its title text; only `spreadsheets get --params '{"ranges":[…],"fields":
  "sheets.data.rowData.values(userEnteredValue,chipRuns,hyperlink)"}'` shows the chip.
- Append: `spreadsheets values append` with `valueInputOption: RAW` (`+append` has no range flag).
- Tab ids: `spreadsheets get --params '{"fields":"sheets.properties(sheetId,title)"}'`; never assume.
- Chips in a cell: `userEnteredValue.stringValue "@"` + `chipRuns[{startIndex:0, chip:{richLinkProperties:{uri}}}]`.
- Structured writes (chips, frozen header, new tab): `spreadsheets batchUpdate` with
  `updateCells` on a `GridRange` (needs `sheetId`, not the tab name).

## Habits

- `--dry-run` on the first batchUpdate of a new shape, then re-fetch: dry-run alone proves nothing.
- **Never delete** a file, row, comment or Doc section. Mark superseded; a human deletes.
- Verify after write: re-fetch and check the change landed before reporting done.
