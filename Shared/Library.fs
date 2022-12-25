namespace Shared

type Car = { Name: string
             Url: string  }

 type ICarsStore = {
    carsfrombase : Async<list<Car>>}


