-- Drop heading identifiers so pandoc's docx writer emits no bookmarks (Drive shows them as icons).
function Header(h) h.identifier = ""; return h end
