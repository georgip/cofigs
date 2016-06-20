#-*- coding: utf-8 -*-
u'''
MOD 10: Functions
'''

# Let's declare a function


def spam():       # Functions are declared with the 'def' keyword, its name,
    print "spam"  # parentheses and a colon. Remember to use indentation!

spam()  # Functions are executed with its name followed by parentheses


# Let's declare a function with arguments


def eggs(arg1):         # Functions arguments go inside the parentheses
    print "eggs", arg1

spam("eggssss")  # Function calls specify arguments inside parentheses


def func(arg1, arg2, arg3):         # There is no limit of arguments
    print "func", arg1, arg2, arg3

func("spam", "eggs", "fooo")


print func("spam", "eggs", "fooo")  # By default functions return None


def my_sum(arg1, arg2):
    return arg1 + arg2    # Use the return keyword to output any result

print my_sum(3, 5)


print my_sum(3.333, 5)
print my_sum("spam", "eggs")  # Given that Python is a dynamic language
                              # we can reuse the same method


print my_sum(arg2="spam", arg1="eggs")  # Use *keyword* arguments to call
                                        # arguments in different order


# Let's declare a function with arguments and default values


def my_pow(arg1, arg2=2):  # It is possible to define default
    return arg1 ** arg2    # values for the arguments, always after
                           # arguments without default values

print my_pow(3)


def my_func(arg1, arg2=2, arg3=3, arg4=4):
    return arg1 ** arg2 + arg3 ** arg4

print my_func(3, arg3=2)  # Use keyword arguments to skip some of the
                          # arguments with default value


# Let's use an arbitrary arguments list


def my_func(arg1=1, arg2=2, *args):  # This arbitrary list is a (kind-off)
    print "> args =", args           # tuple of positional arguments
    return arg1 + arg2

my_func(2, 3)

my_func(2, 3, 5, 7)


my_func(arg1=0, 5, 7)  # Wrong!


spam = (5, 7)
my_func(2, 3, *spam)  # It is possible to unpack a tuple or list as an
                      # arbitrary list of arguments


# The same applies for arbitrary keyword arguments


def my_func(arg1=1, arg2=2, **kwargs):  # The arbitrary 'kwargs' is a
    print "> kwargs =", kwargs          # dictionary of *keyword* args.
    return arg1 + arg2

my_func(2, 3)

my_func(2, 3, param3=5, param4=7)

spam = {"param3": 5, "param4": 7}
my_func(2, 3, **spam)  # It is possible to unpack a tuple or list as an
                       # arbitrary list of arguments


# Functions are first classed objects

def function_caller(f):
    f()


def func_as_arg():
    print 'There should be one-- and preferably only one --obvious way to do it.'

function_caller(func_as_arg)  # Functions can be passed as arguments


#===============================================================================
# REMEMBER:
#     - Functions are declared with the 'def' keyword, its name, parentheses and a colon
#         - Specify arguments inside the parentheses
#         - Define arguments' default values with an equal, after arguments without def val
#         - Specify arbitrary arguments or keyword arguments with *args or **kwargs
#            - Actually only the asterisks matter, the name is up to you
#     - Use indentation for the body of the function, typically 4 spaces per level
#     - Functions are executed with its name followed by parentheses
#         - Provide input arguments inside the parentheses
#         - Provide keywords arguments specifying their name
#     - Functions can be declared and called outside classes
#     - Functions are first classed objects
#         - You can pass them as arguments
#===============================================================================


#===============================================================================
# SOURCES:
#  - http://docs.python.org/2/tutorial/controlflow.html#defining-functions
#  - http://docs.python.org/2/tutorial/controlflow.html#more-on-defining-functions
#  - http://docs.python.org/2/reference/compound_stmts.html#function
#===============================================================================
