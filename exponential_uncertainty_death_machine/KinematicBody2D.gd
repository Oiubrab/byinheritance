extends KinematicBody2D
#this script controls movement

onready var screen_size = get_viewport_rect().size

#movement
var motion = Vector2()
var speed = 10

#time
var sequence = 0
var epoch_random = 1
var sequence_upper = 1000

#random
var rng = RandomNumberGenerator.new()

#position record 
var rotation_random
var cat_pos
var is_size
var mouse_direction
var mouse_direction_left
var mouse_direction_right

#cat sense
var cat_left
var cat_right

#game record
var times_eaten = 5

#testing
var human
var inout
var trigger = false

#ai
var shift=0
var speed_shift=0
var collision_printer
var performance_counter = 0
var crazy = 0







#player controlled motions

func add_motion_rotational():
	
	var ai_turn_mag=200.0
	
	if Input.is_action_pressed("ui_right"):
		rotation += PI/20
		
	elif Input.is_action_pressed("ui_left"):
		rotation -= PI/20
		
	elif shift != 0:
		rotation += PI*(float(shift)/ai_turn_mag)

		
	if Input.is_action_pressed("ui_up"):
		speed += 1
	elif speed_shift>0:
		speed += 1
		
	if Input.is_action_pressed("ui_down"):
		speed -= 1
	elif speed_shift<0:
		speed -= 1



#auto motions

#mouse random movement
func auto_motion_rotational():
	rng.randomize()
	#if the randomiser selects the right number, set another gaussian profile
	if (sequence%int(epoch_random)==0):
		rotation_random = rng.randf_range(-1.0, 1.0)
		epoch_random = rng.randf_range(1, 300)
	#otherwise, make the thing spin
	else:
		var gauss = (float(sequence%int(epoch_random))-(float(epoch_random)/2.0))/float(epoch_random/8)
		var gaussian = exp(-1.0*pow(gauss,2))
		rotation += (PI/16)*rotation_random*gaussian

#everything sways
func disturb():
	if (sequence%sequence_upper > sequence_upper/2):
		rotation +=PI/3600
	else:
		rotation -=PI/3600

#mouse moves faster when player gets closer
func auto_motion_linear():
	var cat_position = get_node("../cat").get("position")
	var cat_distance = position.distance_to(cat_position)
	#food moves some fraction of maximum speed
	speed = 20000*(1.0/cat_distance)

#randomise the position
func reset_life():
	times_eaten += 1
	get_node("../mouse").rotation = rng.randf_range(-PI, PI)
	get_node("../mouse").position = Vector2(rng.randf_range(128, 896),rng.randf_range(128, 448))





#collision controler

#collision between cat and food test
func catsprung():
	#first, check if the mouse has just been caught
	if (collision_printer!="mouse"):
		#find the collision
		for i in get_slide_count():
			var collision = get_slide_collision(i)
			if (collision.collider.name=="mouse"):
				collision_printer = "mouse"
			else:
				collision_printer = " "
		#reset the food
		if (collision_printer=="mouse"):
			reset_life()
	#if mouse was just caught, let it go
	else:
		collision_printer = " "




#direction control

func move_in():
	motion.x = sin(rotation)*speed
	motion.y = -1*cos(rotation)*speed

func rotation_finder():
	#rotate sprite for cardinal directions
	if (motion.x == speed):
		rotation_degrees = 90
	if (motion.x == -speed):
		rotation_degrees = -90
	if (motion.y == speed):
		rotation_degrees = 180
	if (motion.y == -speed):
		rotation_degrees = -360
		
	#rotate sprite for sub-cardinal directions
	if ((motion.x == speed) and (motion.y==-speed)):
		rotation_degrees = 45
	if ((motion.x == speed) and (motion.y==speed)):
		rotation_degrees = 135
	if ((motion.x == -speed) and (motion.y==speed)):
		rotation_degrees = 225
	if ((motion.x == -speed) and (motion.y==-speed)):
		rotation_degrees = -45



#network control and logging

#this function builds an array and sets an entry based on the mouse direction
func where_mouse(cat_position):
	if (str(get_node(".").name)=="cat"):
		var mouse_position = get_node("../mouse").get("position")
		var angle_cat_to_mouse
		var angle_cat = get_node("../cat").get("rotation")
		var angle_mouse = -1*atan2(cat_position.x - mouse_position.x , cat_position.y - mouse_position.y )
		#special case as value jumps to 2pi
		if (abs(angle_cat)>4.0):
			angle_cat=0.0
		#piecewise function to calculate angle because its 3am
		if (angle_cat<=0 and angle_mouse<=0):
			angle_cat_to_mouse  = abs(angle_cat)-abs(angle_mouse)
		elif (angle_cat>=0 and angle_mouse>=0):
			angle_cat_to_mouse  = abs(angle_mouse)-abs(angle_cat)
		else:
			var try_up = abs(angle_cat) + abs(angle_mouse)
			var try_down = (PI - abs(angle_cat)) + (PI - abs(angle_mouse))
			if (try_up<try_down):
				angle_cat_to_mouse = try_up
				if (angle_cat>0):
					angle_cat_to_mouse = -1*angle_cat_to_mouse
			elif (try_down<try_up):
				angle_cat_to_mouse = try_down
				if (angle_cat<0):
					angle_cat_to_mouse = -1*angle_cat_to_mouse
		return angle_cat_to_mouse

func general_type_of(obj):
	var typ = typeof(obj)
	var builtin_type_names = ["nil", "bool", "int", "real", "string", "vector2", "rect2", "vector3", "maxtrix32", "plane", "quat", "aabb",  "matrix3", "transform", "color", "image", "nodepath", "rid", null, "inputevent", "dictionary", "array", "rawarray", "intarray", "realarray", "stringarray", "vector2array", "vector3array", "colorarray", "unknown"]

	if(typ == TYPE_OBJECT):
		obj.type_of()
	else:
		builtin_type_names[typ]




func _notification(what):
	if what == MainLoop.NOTIFICATION_WM_QUIT_REQUEST:
		var some = File.new()
		some.open("../thing.txt", File.WRITE)
		some.store_string("this")
		some.close()
		get_tree().quit() # default behavior

#this is where the magic happens

#initialisation function
func _ready():
	rng.randomize()
	reset_life()
	move_in()
	rotation_finder()
	var nothing = []
	#var train = OS.execute("rm will.txt",[],false)
	get_tree().paused = true
	#get the size of the object
	is_size = get_node("CollisionShape2D").get_shape().get_radius()
	human=true
	inout=false
	crazy = OS.execute("rm", ["../thing.txt"])
	crazy = OS.execute("rm", ["../machine.txt"])

#the main road
func _physics_process(_delta):
	
	sequence += 1
	
	#turn ai on and off
	if Input.is_action_just_pressed("ui_accept"):
		if human==false:
			human=true
			#notify the ai that human is now on
			crazy = OS.execute("rm", ["../machine.txt"])
			
		elif human==true:
			human=false

			#notify the ai that human is now off
			var live = File.new()
			live.open("../machine.txt", File.WRITE)
			live.store_string("this")
			live.close()
	
	#take the shift from the network
	if (str(get_node(".").name)=="cat"):
		var output = []
		var operate = OS.get_name()

		
		#establish the position of the cat and its eye
		cat_pos = get_node("../cat").get("position")
		cat_left=Vector2(cat_pos.x-sin((PI/2)-rotation)*is_size,cat_pos.y-cos((PI/2)-rotation)*is_size)
		cat_right=Vector2(cat_pos.x+sin((PI/2)-rotation)*is_size,cat_pos.y+cos((PI/2)-rotation)*is_size)
		
		#send direction to mouse to labels
		mouse_direction=where_mouse(cat_pos)
		mouse_direction_left=where_mouse(cat_left)
		mouse_direction_right=where_mouse(cat_right)
		
		
		#run the ai and get the shift
		if human==false:

			#var sane = OS.execute("kill",[str(crazy)],false)
			#binocular vision
			
			var locker
			
			#input response getter
			locker = File.new()
			locker.open("../response_lock.txt", File.WRITE)
			locker.close()
			var testicle=File.new()
			testicle.open("../alison_hell/response.csv",File.READ)
			var tester=testicle.get_csv_line()
			testicle.close()
			crazy = OS.execute("rm", ["../response_lock.txt"])
			
			#left array builder
			locker = File.new()
			locker.open("../sight_left_lock.txt", File.WRITE)
			locker.close()
			var fuck_left = File.new()
			fuck_left.open("../alison_hell/sight_left.csv", File.WRITE)
			var mouse_left=[]
			var trigger=false
			for halycon in range(45):
				if (2*PI/45.0)*halycon>mouse_direction_left+PI and trigger==false:
					trigger=true
					mouse_left=mouse_left+[1]
				else:
					mouse_left=mouse_left+[0]
					
			fuck_left.store_csv_line(mouse_left)
			fuck_left.close()
			crazy = OS.execute("rm", ["../sight_left_lock.txt"])
			
			#right array builder
			locker = File.new()
			locker.open("../sight_right_lock.txt", File.WRITE)
			locker.close()
			var fuck_right = File.new()
			fuck_right.open("../alison_hell/sight_right.csv", File.WRITE)
			var mouse_right=[]
			trigger=false
			for halycon in range(45):
				if (2*PI/45.0)*halycon>mouse_direction_right+PI and trigger==false:
					trigger=true
					mouse_right=mouse_right+[1]
				else:
					mouse_right=mouse_right+[0]
					
			fuck_right.store_csv_line(mouse_right)
			fuck_right.close()
			crazy = OS.execute("rm", ["../sight_right_lock.txt"])
			
			#food array builder
			locker = File.new()
			locker.open("../food_lock.txt", File.WRITE)
			locker.close()
			var titfuck = File.new()
			titfuck.open("../alison_hell/food.csv",File.WRITE)
			var food_array=[]
			for halycon in range(5):
				if halycon==times_eaten or (halycon==4 and times_eaten>4):
					food_array=food_array+[1]
				else:
					food_array=food_array+[0]
			titfuck.store_csv_line(food_array)
			titfuck.close()
			crazy = OS.execute("rm", ["../food_lock.txt"])
			
			#find the shift
			for pos in range(20):
				if int(tester[pos])==1:
					shift=pos-10
					break

		else:
			shift=0
			speed_shift=0
		
	#this is the cat
	if (str(get_node(".").name)=="cat"):
		add_motion_rotational()
		catsprung()
		move_in()

		
	#this is the mouse
	elif (str(get_node(".").name)=="mouse"):
		auto_motion_linear()
		auto_motion_rotational()
		move_in()

	
	rotation_finder()
	disturb()
	
	motion = move_and_slide(motion)
