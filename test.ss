var myname = fn () {
  return this.name
}

var printname = fn () {
  console.log(this.myname())
}

var person1 = {name: "Fred", age: 21, myname: myname, printname: printname}

person1.printname()
