module Config where

-- One second in microseconds
interval :: Int
interval = 1000000

-- port number
serverPort :: Int
serverPort = 65535

-- time to die constant in seconds. need to change to option later
deathCounter :: Int
deathCounter = 10

-- server root
serverRoot = "/home/alacrity/files/"
