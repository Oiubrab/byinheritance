extends Camera2D


# Declare member variables here. Examples:
# var a = 2
# var b = "text"

func zoom():
	if Input.is_action_just_released('wheeldown'):
		zoom.x += 0.25
		zoom.y += 0.25
	if Input.is_action_just_released('wheelup') and zoom.x > 1 and zoom.y > 1:
		zoom.x -= 0.25
		zoom.y -= 0.25

# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	zoom()
