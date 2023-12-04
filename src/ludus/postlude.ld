& this file runs after any given interpretation
& the goal is to output any global state
& this does not have base loaded into it: must be pure ludus

if turtle_state() :visible? then render_turtle! () else nil

let console_msgs = flush! ()

let (r, g, b, a) = deref (bgcolor)
make! (bgcolor, colors :black)

let draw_calls = deref (p5_calls)
make! (p5_calls, [])

#{
	& :result result is provided elsewhere
	& :errors [] & if we get here there are no errors
	:console console_msgs
	:draw concat (
		[(:background, r, g, b, a), (:stroke, 255, 255, 255, 255)]
		draw_calls)
}
