-- These initials cover Jake's name variants in the bibliography.
local authorInitials = { J = true, CJ = true, CJD = true }

local function boldAuthor(span)
  if not span.classes:includes("csl-right-inline") then
    return nil
  end

  local result = pandoc.List()
  local inAuthors = true
  local i = 1
  while i <= #span.content do
    local word = span.content[i]
    local space = span.content[i + 1]
    local name = span.content[i + 2]
    local initials, punctuation
    if inAuthors and word.t == "Str" and word.text == "Barlow"
        and space and space.t == "Space" and name and name.t == "Str" then
      initials, punctuation = name.text:match("^(%u+)([,%.]?)$")
    end

    if initials and authorInitials[initials] then
      result:insert(pandoc.Strong({ word, space, pandoc.Str(initials) }))
      if punctuation ~= "" then result:insert(pandoc.Str(punctuation)) end
      if punctuation == "." then inAuthors = false end
      i = i + 3
    else
      result:insert(word)
      -- The site's Vancouver style ends the author list with a full stop.
      if word.t == "Str" and word.text:match("%.$") then inAuthors = false end
      i = i + 1
    end
  end
  span.content = result
  return span
end

function Pandoc(doc)
  -- Quarto normally runs citeproc after user filters. Run it here so the
  -- formatted bibliography is available, with automatic citeproc disabled.
  doc = pandoc.utils.citeproc(doc)
  return doc:walk({
    Div = function(div)
      if div.identifier == "refs" then
        return div:walk({ Span = boldAuthor })
      end
    end
  })
end
