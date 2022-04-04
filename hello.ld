fn explode_if_ten {
	(10) -> panic! ("That was ten")
	(x) -> print ("Ok, not ten!")
}

explode_if_ten (8)

explode_if_ten (10)