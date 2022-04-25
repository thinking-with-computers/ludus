import "bar.ld" as Bart

print ("Bar :foo should be :foo. It is ", Bart :foo)

print ("But if I do something naughty like accessing something that isn't in an ns, Ludus will panic")

Bart :quux