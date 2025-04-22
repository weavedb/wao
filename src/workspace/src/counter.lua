local count = 0

Handlers.add("Inc", "Inc", function (msg)
  count = count + 1
  msg.reply({ Data = "Incremented!" })
end)

Handlers.add("Get", "Get", function (msg)
  msg.reply({ Data = tostring(count) })
end)
