// sicp solution ch 1.1 by okie
// just translated from cl version. need to be improved javascriptly.
// tested with mozREPL on FireFox.

// ex 1.2
(5 + 4 + (2 - (3 - (6 + 4 / 5)))) / (3 * (6 - 2) * (2 - 7));

// ex 1.3
function square(x)
{
  return (x * x);
}

function sum_of_two_squares (x,y)
{
  return square(x) + square(y);
}

// solution 1
function sum_of_larger_two (x, y, z)
{
  if (x > y) 
    return (y > z) ? sum_of_two_squares(x, y) : sum_of_two_squares(x, y);
  else 
    return (x > z) ? sum_of_two_squares(y, x) : sum_of_two_squares(y, z);
}

// soluation 2
function sum_of_larger_two (x, y, z)
{
  var bigger;
  if (x >= y) {
    bigger = (y >= z) ? y : z;
    return sum_of_two_squares (x, bigger);
  }
  else {
    bigger = (x >= z) ? x : z;
    return sum_of_two_squares (y, bigger);
  }
}

// solution 3
function smallest_of_three (x, y, z)
{
  return (x > y) ? ((y > z) ? z : y) : ((x > z) ? z : x);
}

function sum_of_larger_two (x, y, z) 
{
  return square(x) + square(y) + square(z) - square(smallest_of_three(x, y, z));
}

// solution 4
function sum_of_larger_two (a, b, c)
{
  return ((a > b) || (a > c) ? square(a) : 0)
    + ((b > c) || (b > a) ? square(b) : 0)
    + ((c > a) || (c > b) ? square(c) : 0);
}

// ex 1.4
function a_plus_abs_b (a, b)
{
  return (b > 0) ? a + b : a - b;
}

// ex 1.5
function p()
{
  p();
}
// !!! InternalError: too much recursion

function test(x, y)
{
  if (x === 0)
    return 0;
  else
    return y;
}

test(0, p());

// ex 1.6
// javascript only supports binary floating point number. sucks!
// TODO : find workarounds.
function average (x, y)
{
  return (x + y) / 2;
}

function good_enough (guess, x)
{
  return Math.abs(square(guess) - x) < 0.001 ? true : false;
}

function improve (guess, x)
{
  return average(guess, (x / guess));
}

function sqrt_iter (g, x)
{
  if (good_enough(g, x))
    return g;
  else
    return sqrt_iter(improve(g,x), x);
}

function mysqrt (x)
{
  return sqrt_iter(1.0, x);
}

//repl> mysqrt(25)
//5.000023178253949

// ex 1.7
// solution 1
function guess_enough (g, prev_guess)
{
  return Math.abs(g-prev_guess) < (g * 0.001);
}

function sqrt_iter (g, prev_guess, x)
{
  if (guess_enough(g, prev_guess))
    return g;
  else
    return sqrt_iter(improve(g, x), g, x);
}

function mysqrt (x)
{
  return sqrt_iter (1.0, 0.0, x);
}

//repl> mysqrt(25)
//5.000000000053722

// solution 2
function close_enough (a, b)
{
  return Math.abs(1 - (a/b)) < 0.001;
}

function sqrt_iter (g, x)
{
  var improved_guess = improve(g, x);
  if (close_enough(g, improved_guess))
    return improved_guess;
  else
    return sqrt_iter(improved_guess, x);
}

function mysqrt (x)
{
  return sqrt_iter(1.0, x);
}

//repl> mysqrt(25)
//5.000000000053722

// ex 1.8
function cbrt_improve (guess, x)
{
  return (((x / (guess * guess)) + (2 * guess)) / 3);
}

function cbrt_iter (g, x)
{
  var improved_guess = cbrt_improve(g, x);
  if (close_enough(g, improved_guess))
    return improved_guess;
  else
    return cbrt_iter(improved_guess, x);
}

function cbrt (x)
{
  return cbrt_iter(1.0, x);
}

//repl> cbrt(27)
//3.0000005410641766
