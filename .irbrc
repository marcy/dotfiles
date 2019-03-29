# -*- mode: ruby -*-

require "rubygems"
require "wirble"
require "irb/completion"
require "nkf"
require "what_methods"
# require "irbtools"

IRB.conf[:PROMPT][:CODE] = {
  :PROMPT_I => " ",
  :PROMPT_N => " ",
  :PROMPT_S => "#=> ",
  :PROMPT_C => nil,
  :RETURN => " #=> %s\n"
}

Wirble::init
Wirble::colorize

def codemode!
  conf.prompt_mode = :CODE
end

module Kernel
  def r(arg)
    puts NKF.nkf("-Ew", `refe #{arg}`)
  end
  private :r
end

class Module
  def r(meth = nil)
    if meth
      if instance_methods(false).include? meth.to_s
        puts NKF.nkf("-Ew", `refe #{self}##{meth}`)
      else
        super
      end
    else
      puts NKF.nkf("-Ew", `refe #{self}`)
    end
  end
end
