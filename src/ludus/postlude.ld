& this file runs after any given interpretation
& even if the original interpretation panics
& the goal is to output any global state held in Ludus
& this does not have base loaded into it, only prelude: must be pure Ludus

if turtle_state() :visible? then render_turtle! () else nil

make! (turtle_states, [turtle_init])

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
