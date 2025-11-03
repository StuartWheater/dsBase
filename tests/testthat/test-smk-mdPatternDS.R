#-------------------------------------------------------------------------------
# Copyright (c) 2025 ProPASS Consortium. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("mdPatternDS::smk::setup")

#
# Tests
#

context("mdPatternDS::smk::sample data.frame")
test_that("mdPatternDS: sample data.frame", {
    x_val <- data.frame(v1 = c(0.0, 1.0, 2.0, 3.0, 4.0), v2 = c(4.0, 3.0, 2.0, 1.0, 0.0))
    x     <- "x_val"

    res <- mdPatternDS(x)

    expect_length(res, 3)
})

#
# Done
#

context("mdPatternDS::smk::shutdown")

context("mdPatternDS::smk::done")
