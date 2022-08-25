extends Sprite


# Declare member variables here. Examples:
var colour_caster = 1
export var strobe_speed = 100
var food
var food_before = 0
# var b = "text"


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	
	#only strobe the cat
	if (str(get_node("..").name)=="cat"):
		if (colour_caster%(3*strobe_speed)==0):
			modulate = Color(0, 1, 0) 
			if (strobe_speed/2>0):
				strobe_speed = strobe_speed/2
		elif (colour_caster%(3*strobe_speed)==1*strobe_speed):
			modulate = Color(1, 0, 0) 
		elif (colour_caster%(3*strobe_speed)==2*strobe_speed):
			modulate = Color(0, 0, 1) 
			#effects of starvation
			#belly is the variable used for the number of times the cat has eaten the mouse
			var belly = get_node("../../cat").get("times_eaten")
			if belly>0:
				get_node("../../cat").times_eaten = belly-1
		colour_caster += 1
		
		food = get_node("..").times_eaten
		if (food_before != food):
			modulate = Color(1 ,1 ,1)
			strobe_speed=100
		food_before=food
