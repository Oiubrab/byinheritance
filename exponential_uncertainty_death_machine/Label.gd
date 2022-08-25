extends Label


# Declare member variables here. Examples:
# var a = 2
var b = "Die on angle: "
var player_ai_rotate

# Called when the node enters the scene tree for the first time.
func _ready():
	player_ai_rotate=0
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	player_ai_rotate = get_node("../player").get("rotation")
	set_text(b+str(player_ai_rotate))
