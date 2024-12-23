local json = require("json")
function rollup (id)
  local file = io.open("/rollup/" .. id)
  local data = nil
  if file then data = file:read(file:seek('end')) end
  file:close()
  return data
end

function get (col, doc)
  local file = io.open("/data/" .. col .. "/" .. doc)
  local data = nil
  if file then data = file:read(file:seek('end')) end
  file:close()
  return data
end

Handlers.add(
  "Rollup",
  "Rollup",
  function (msg)
    msg.reply({ Data = "committed!" })
  end
)

Handlers.add(
  "Finalize",
  "Finalize",
  function (msg)
    local data = json.decode(rollup(msg.TXID))
    msg.reply({ Data = "finalized!" })
  end
)


Handlers.add(
  "Get",
  "Get",
  function (msg)
    msg.reply({ Data = get(msg.col, msg.doc) })
  end
)

