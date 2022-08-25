extends RichTextLabel


# Declare member variables here. Examples:
var ang_left = "Angle to Mouse (left): "
var ang_right = "Angle to Mouse (right): "
var foody = "food: "
var hater = "Player: "
var player_ai_rotate_left
var player_ai_rotate_right
var humanity

var foodie

# Called when the node enters the scene tree for the first time.
func _ready():
	player_ai_rotate_left=0
	player_ai_rotate_right=0
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):

	player_ai_rotate_left = get_node("../cat").get("mouse_direction_left")
	player_ai_rotate_right = get_node("../cat").get("mouse_direction_right")
	humanity = get_node("../cat").get("human")

	foodie = get_node("../cat").get("times_eaten")
	set_text(ang_left)
	add_text(str(player_ai_rotate_left))
	newline()
	add_text(ang_right+str(player_ai_rotate_right))
	newline()
	add_text(foody+str(foodie))
	newline()
	if humanity==true:
		add_text(hater+"human")
	else:
		add_text(hater+"AI")
