
# Note: To use the "r-user" AppArmor profile you should add the following line
# to /etc/apparmor.d/rapparmor.d/r-user:
#
#  /usr/lib/rstudio/bin/pandoc/* rix,
#

options(tutorial.exercise.evaluator.onstart = function(pid) {

  # import RAppArmor
  require(RAppArmor, quietly = TRUE)

  # set process group to pid (allows kill of entire subtree in cleanup)
  setpgid();
  
  # set nice priority
  setpriority(10)
  
  # set rlimits as appropriate
  rlimit_nproc(1000)
  rlimit_as(1024*1024*1024)
  
  # change to r-user profile (see note above on required edit to r-user)
  aa_change_profile("r-user")
})

options(tutorial.exercise.evaluator.oncleanup = function(pid) {
  
  # import RAppArmor
  require(RAppArmor, quietly = TRUE)
  
  # kill entire process subtree. note that the second call works
  # because the call to setpgid above sets our pgid (process group id)
  # to our pid (process id)
  kill(pid, tools::SIGKILL)
  kill(-1 * pid, tools::SIGKILL)
})

