mel((usersData,full), [docker], 64, []).

mel((videoStorage,full), [docker], 16, []).
mel((videoStorage,medium), [docker], 8, []).

mel((movementProcessing,full), [docker], 8, []).
mel((movementProcessing,medium), [gcc, make], 4, []).

mel((arDriver,full), [docker], 4, [phone, lightSensor]).
mel((arDriver,medium), [gcc,caffe], 2, [phone, lightSensor]).
mel((arDriver,light), [gcc], 1, [phone]).

mel2mel(usersData, videoStorage, 70).
mel2mel(videoStorage, movementProcessing, 30).
mel2mel(movementProcessing, arDriver, 20).

application((arApp, full), [(usersData,full), (videoStorage,full), (movementProcessing,full), (arDriver,full)]).
application((arApp, adaptive), [(usersData,full), (videoStorage,_), (movementProcessing,_), (arDriver,_)]).