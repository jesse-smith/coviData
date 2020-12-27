# 1. Update
# 1.1 Type and run
      devtools::update_packages()
# 1.2 Enter `2` for CRAN-only updates (we'll install the rest later)
# 1.3 Say `No` when asked whether you want to compile packages from source
#     (we'll install these later too)
# 1.4 Wait for devtools to throw an error.
#     If it doesn't, you've been blessed by the gods.
#     When it does, move to step 2.

# 2. Fix
# 2.1 Note which package could not be installed; we'll call this `annoying_pkg`
# 2.2 Restart R (`Session -> Restart R`)
# 2.3 Type and run
      install.packages("annoying_pkg") # (replace `annoying_pkg` with the problem package)
#     This should install the package without error.
# 2.5 If it does, great. Move to step 3.
#     If not... still move to step 3 and hope you don't need it too badly.
#     We'll have to handle packages that don't install like this individually.

# 3. Rinse and Repeat
# 3.1 Now repeat steps 1 and 2 until either all packages are installed
#     or you can't make any further progress.
