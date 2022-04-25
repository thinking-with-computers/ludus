import "bar.ld" as Bar

print ("Bar :foo should be :foo. It is ", Bar :foo)

print ("But if I do something naughty, Ludus will panic")

panic! ("oops", :foo, 42)