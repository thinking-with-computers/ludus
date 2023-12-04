& this file runs after any given interpretation
& the goal is to output any global state

if turtle_state() :visible? then render_turtle! () else nil

#{
	& :result result is provided elsewhere
	:errors [] & if we get here there are no errors
	:console deref (console)
	:draw deref (p5_calls)
}