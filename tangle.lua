blocks={}

function keep(classes,targets)
  for k,_ in pairs(targets) do
    if not classes[k] then
      return false
    end
  end
  return true
end

function split_classes(str)
  classes = {}
  for class in string.gmatch(str, "[^ ]+") do
    classes[class] = true
  end
  return classes
end

function Doc(body, metadata, variables)
  targets = split_classes(metadata.code or '')
  for ix,chunk in pairs(blocks) do
    if keep(chunk[1],targets) then
      blocks[ix] = chunk[2]
    else
      blocks[ix] = ''
    end
  end
  return table.concat(blocks,'\n')
end

function CodeBlock(s, attr)
  if attr.class then
    table.insert(blocks,{split_classes(attr.class),s})
  end
  return ""
end

function Ignore()
  return ""
end

-- The following code ignores any other items
local meta = {}
meta.__index =
  function(_, key)
    -- io.stderr:write(string.format("WARNING: Undefined function '%s'\n",key))
    _G[key] = Ignore
    return Ignore
  end
setmetatable(_G, meta)
