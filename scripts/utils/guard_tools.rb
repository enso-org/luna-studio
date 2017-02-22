require 'open3'
require 'colorize'  # What a travesty, indeed. This shall be spelled 'colourise'. Bloody colonies!
require 'artii'
require 'pty'

## Prevent the script to run multiple times in a row for a file saved multiple times
#
# The thing is that `guard` stores actions in FIFO and preforms them all. This global time variable
# is here to short-circuit such cases.

$lastbuild    = Time.now

def lastbuildguard(trigger, &block)
  begin
    sleep 1.0/10.0  # arbitrary small sleep to ensure the "save all" is done
    if File.mtime(trigger) < $lastbuild
      puts "File #{trigger} modified before last build, so rebuilding is unnecessary".starsaround.green
    else
      $lastbuild = Time.now
      puts "\nBuilding at #{$lastbuild}\n"
      block.call()
      puts "\nBuild finished at #{Time.now}\n"
    end
  rescue SystemCallError => e
    puts "The run triggered by #{trigger} caused an error"
  end
end


## String various operations
#
# For formatting purposes.
# YES, I KNOW that monkey patching of base classes is BAD. However I believe that this is simple enough to not cause
# any harm. If I'm mistaken... well... Sorry :<

class String
  @@linefill_length = 121

  def self.linefill_length
    @@linefill_length
  end

  def starfill
    x = self
    if length % 2 != @@linefill_length % 2
      x = x + " "
    end
    x.ljust(@@linefill_length, " *")
  end

  def linefill
    ljust(@@linefill_length, '-')
  end

  def self.starline
    "*".ljust(@@linefill_length, " *")
  end

  def starsaround
    stars = "*".ljust(@@linefill_length, " *")
    replace (stars + "\n" + self + "\n" + stars)
  end

  def starsallaround
    starfill.starsaround
  end

  # This removes the colour-codes from the string. It's handy quite often.
  def nocolourcodes
    gsub(/\e\[(\d+)(;\d+)*m/,'')
  end

  # Used for mulitline strings with variable line length
  def center_with_strlen(strlen)
    len = @@linefill_length - strlen
    len = 0 if len < 0
    " " * (len / 2) + self
  end
end


## Run a command
#
# Run command with all the fanciness of it. Measure execution time, print stdout+stderr, shows the return code.
def command_interactive(cmd)
  puts String.starline.blue
  puts ("$ #{cmd}".blue + " 2>&1".yellow)

  stdout_empty = true
  
  start = Time.now
  begin
    PTY.spawn( cmd ) do |stdin, stdout, pid|
      begin
        puts "* * STDOUT & STDERR".starfill.blue
        stdin.each { |line| 
          stdout_empty = false
          puts "  * ".blue + line
        }
      rescue Errno::EIO
      end
      Process.wait(pid)
    end
  rescue PTY::ChildExited
    puts "  * Strange, the child process exited...".starsaround.red
  end
  finish = Time.now

  puts "  * ".blue + "STDOUT: none".black if stdout_empty

  diff = finish - start
  puts String.starline.blue
  if diff > 5 then puts ("  * ".blue + ("exec time: %.2f sec" % diff))
              else puts ("  * ".blue + ("exec time: %.2f sec" % diff).black)
  end

  if $?.exitstatus === 0  then puts ("  * ".blue + "exit code: #{$?.exitstatus}".black)
                          else puts "  * exit code: #{$?.exitstatus}".starsaround.red
  end

  if $?.exitstatus != 0
    unless $!.nil?
      raise SystemCallError.new("Execution of `#{cmd}` failed with error code: #{$!.exitstatus}")
    else
      raise SystemCallError.new("Execution of `#{cmd}` failed")
    end
  end
end

## Run a command
#
# Run command with all the fanciness of it. Measure execution time, print stdout and stderr in appropriate colours,
# shows the return code. Also passes input (stdin) if provided.
def command_withinput(cmd, inp=nil)
  puts "$ #{cmd}".starsaround.blue

  start = Time.now
  stdout, stderr, status = Open3.capture3(cmd, :stdin_data=>inp)
  finish = Time.now
  diff = finish - start

  if diff > 5 then puts ("  * ".blue + ("exec time: %.2f sec" % diff))
              else puts ("  * ".blue + ("exec time: %.2f sec" % diff).black)
  end

  if status.exitstatus === 0 then puts ("  * ".blue + "exit code: #{status.exitstatus}".black)
                             else puts ("  * ".blue + "exit code: #{status.exitstatus}".light_white)
  end

  unless stdout.empty?
    puts "* * STDOUT".starfill.green
    puts stdout.each_line.map {|l| "  * ".green + l}.join
  else
    puts "  * ".green + "STDOUT: none".black
  end

  unless stderr.empty?
    puts "* * STDERR".starsallaround.red
    puts stderr.each_line.map {|l| "  * ".red + l}.join
  end

  if status.exitstatus != 0
    raise SystemCallError.new("Execution of `#{cmd}` failed with error code: #{status.exitstatus}")
  end
end


## Pretty-printer of sections
#
# Can run commands if passed.
# TODO (feature): could take a block and measure its execution time.

def section(name, *cmds, condition: true, noexception: false, &block)
  if condition
    begin
      grace = Artii::Base.new :font => 'graceful'
      ascii_lines = grace.asciify(name).each_line
      ascii = ascii_lines.map { |line| line.center_with_strlen( ascii_lines.map(&:length).max ) }.join.cyan

      puts "".linefill.cyan
      puts ascii.cyan
      puts "".linefill.cyan

      cmds.map do |c| command_withinput(c) end

      block.call() unless block.nil?
    rescue SystemCallError => e
      unless noexception
        newe = $!.exception("Error in section '#{name}'\n#{$!}")
        raise newe
      end
    end
  end
end


def noSystemCallError(&block)
  begin
    block.call()
  rescue SystemCallError => e
  end
end
