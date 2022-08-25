extends Node



func _ready():
	pass

func _process(delta):
	pass

func _notification(what):
	if what == MainLoop.NOTIFICATION_WM_QUIT_REQUEST:
		get_tree().quit() # default behavior
		var insanity = OS.execute("rm will.txt",[])
