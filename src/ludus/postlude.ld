& this file runs after any given interpretation
& the goal is to output any global state

if turtle_state() :visible? then render_turtle! () else nil

let console_msgs = deref (console)
set! (console, [])

let (r, g, b, a) = deref (bgcolor)
set! (bgcolor, colors :black)

let draw_calls = deref (p5_calls)
set! (p5_calls, []) 

#{
	& :result result is provided elsewhere
	& :errors [] & if we get here there are no errors
	:console console_msgs
	:draw concat ([(:background, r, g, b, a)], draw_calls)
}