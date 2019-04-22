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

local function keyCodeSet(keys)
   return function()
      for i, keyEvent in ipairs(keys) do
         keyEvent()
      end
   end
end

local function remapKey(mods, key, keyCode)
   hs.hotkey.bind(mods, key, keyCode, nil, keyCode)
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

local disableApps = {"Emacs", "iTerm2", "ターミナル", "RubyMine", "Inkdrop", "Boostnote", "Hyper"}

local function isDisableApp(name)
   -- hs.alert.show(name)
   for index = 1, #disableApps do
      if disableApps[index] == name then
         return true
      end
   end

   return false
end


local function handleGlobalEvent(name, event, app)
   if event == hs.application.watcher.activated then
      if isDisableApp(name) then
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

-- remapKey({"ctrl"}, "m", keyCode("return"))
remapKey({"ctrl"}, "j", keyCode("return"))

remapKey({"ctrl"}, "w", keyCode("x", {"cmd"}))
remapKey({"ctrl"}, "y", keyCode("v", {"cmd"}))

remapKey({"ctrl"}, "h", keyCode("delete"))
remapKey({"ctrl"}, "k", keyCodeSet({
         keyCode('right', {'cmd', 'shift'}),
         keyCode('x', {'cmd'}),
         keyCode('forwarddelete')
}))

remapKey({'ctrl'}, 's', keyCode('f', {'cmd'}))
remapKey({'ctrl'}, '/', keyCode('z', {'cmd'}))
remapKey({'ctrl'}, 'g', keyCode('escape'))
remapKey({'ctrl'}, '[', keyCode('escape'))

remapKey({'ctrl'}, 'v', keyCode('pagedown'))
remapKey({'alt'}, 'v', keyCode('pageup'))
remapKey({'cmd', 'shift'}, ',', keyCode('home'))
remapKey({'cmd', 'shift'}, '.', keyCode('end'))

remapKey({'cmd', 'alt'}, 'h', mouseMoveToLeft)
remapKey({'cmd', 'alt'}, 'j', mouseMoveToDown)
remapKey({'cmd', 'alt'}, 'k', mouseMoveToUp)
remapKey({'cmd', 'alt'}, 'l', mouseMoveToRight)

remapKey({'cmd', 'alt'}, 'v', mouseClickLeft)

-- Cmd-q 2 回押しでアプリ終了する

local quitModal = hs.hotkey.modal.new('cmd','q')

function quitModal:entered()
   hs.alert.show("Press Cmd+Q again to quit", 1)
   hs.timer.doAfter(1, function() quitModal:exit() end)
end

local function killApp()
   hs.console.printStyledtext('killApp')
   hs.application.frontmostApplication():kill()
   quitModal:exit()
end

quitModal:bind('cmd', 'q', killApp)
quitModal:bind('', 'escape', function() quitModal:exit() end)

-- Cmd-q 長押しでアプリ終了する

--[[
local qStartTime = 0.0
local qDuration = 1.5
hs.hotkey.bind({"cmd"}, "Q", function()
    qStartTime = hs.timer.secondsSinceEpoch()
end, function()
    local qEndTime = hs.timer.secondsSinceEpoch()
    local duration = qEndTime - qStartTime
    if duration >= qDuration then hs.application.frontmostApplication():kill() end
end)
]]
