-- counter.lua - Custom Lua module for lua@5.3a device
-- Standalone: no AOS boot code, no Handlers framework.
-- Implements a counter with Inc / Get actions.

local counter = 0

local function intstr(n)
  if n == math.floor(n) then return string.format("%d", n) end
  return tostring(n)
end

function compute(base, req, opts)
  -- Action is inside req.body (the scheduled message)
  local action = nil
  if type(req) == "table" then
    if type(req.body) == "table" then
      action = req.body.Action or req.body.action
    end
    if not action then
      action = req.Action or req.action
    end
  end

  if action == "Inc" then
    counter = counter + 1
  end

  base.results = {
    outbox = {
      ["1"] = { data = intstr(counter) }
    },
    output = ""
  }
  return base
end
