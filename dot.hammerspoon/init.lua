-- remap functions

local function keyCode(key, mods, callback)
   mods = mods or {}
   callback = callback or function() end
   return function()
      hs.eventtap.event.newKeyEvent(mods, string.lower(key), true):post()
      hs.timer.usleep(1000)
      hs.eventtap.event.newKeyEvent(mods, string.lower(key), false):post()
      callback()
   end
end

local function remapKey(mods, key, keyCode)
   hs.hotkey.bind(mods, key, keyCode, nil, keyCode)
end

local function killLine()
   return keyCode("right", {"cmd", "shift"}, keyCode("x", {"cmd"}))
end

-- Mouse Keyboard Setting
local mvOffset = 15

local function mouseMoveToUp()
   absPos = hs.mouse.getAbsolutePosition()
   mvPos = {['x']=absPos['x'], ['y']=absPos['y'] - mvOffset}
   hs.mouse.setRelativePosition(mvPos)
end

local function mouseMoveToDown()
   absPos = hs.mouse.getAbsolutePosition()
   mvPos = {['x']=absPos['x'], ['y']=absPos['y'] + mvOffset}
   hs.mouse.setRelativePosition(mvPos)
end

local function mouseMoveToLeft()
   absPos = hs.mouse.getAbsolutePosition()
   mvPos = {['x']=absPos['x'] - mvOffset, ['y']=absPos['y']}
   hs.mouse.setRelativePosition(mvPos)
end

local function mouseMoveToRight()
   absPos = hs.mouse.getAbsolutePosition()
   mvPos = {['x']=absPos['x'] + mvOffset, ['y']=absPos['y']}
   hs.mouse.setRelativePosition(mvPos)
end

local function mouseClickLeft()
   absPos = hs.mouse.getAbsolutePosition()
   hs.eventtap.leftClick(absPos)
end

-- watch & switch hotkey settings

local function switchHotKeys(enable)
   for k, v in pairs(hs.hotkey.getHotkeys()) do
      if enable then
         v["_hk"]:enable()
      else
         v["_hk"]:disable()
      end
   end
end

local function handleGlobalEvent(name, event, app)
   if event == hs.application.watcher.activated then
      if name == "Emacs" or name == "iTerm2" or name == "ターミナル" then
         switchHotKeys(false)
      else
         switchHotKeys(true)
      end
   end
end

watcher = hs.application.watcher.new(handleGlobalEvent)
watcher:start()

-- remap settings

remapKey({"ctrl"}, "p", keyCode("up"))
remapKey({"ctrl"}, "n", keyCode("down"))
remapKey({"ctrl"}, "f", keyCode("right"))
remapKey({"ctrl"}, "b", keyCode("left"))

remapKey({"ctrl"}, "m", keyCode("return"))
remapKey({"ctrl"}, "j", keyCode("return"))

remapKey({"ctrl"}, "w", keyCode("x", {"cmd"}))
remapKey({"ctrl"}, "y", keyCode("v", {"cmd"}))

remapKey({"ctrl"}, "h", keyCode("delete"))
remapKey({"ctrl"}, "k", killLine())

remapKey({'cmd', 'alt'}, 'h', mouseMoveToLeft)
remapKey({'cmd', 'alt'}, 'j', mouseMoveToDown)
remapKey({'cmd', 'alt'}, 'k', mouseMoveToUp)
remapKey({'cmd', 'alt'}, 'l', mouseMoveToRight)

remapKey({'cmd', 'alt'}, 'v', mouseClickLeft)
