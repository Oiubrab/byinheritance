extends CanvasLayer


# Declare member variables here. Examples:
var triggerd = false
# var b = "text"

# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	#unpause if up is pressed
	if Input.is_action_pressed("ui_up"):
		get_tree().paused = false
	#pause toggle
	elif Input.is_action_just_pressed("ui_end"):
		if get_tree().paused == true:
			get_tree().paused = false
		elif get_tree().paused == false:
			get_tree().paused = true
	
	#pause trigger
	triggerd = get_node("../cat").get("trigger")
	if triggerd==true:
		get_tree().paused = true
