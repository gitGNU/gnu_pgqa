# Copyright (C) 2016 Antonin Houska
#
# This file is part of PGQA.
#
# PGQA is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# PGQA is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License qalong
# with PGQA. If not, see <http://www.gnu.org/licenses/>.

EMACSLOADPATH = :./lisp/pgqa/
INPUT_DIR = ./test/input/
OUTPUT_DIR = ./test/output/
CHECK_DIR = ./test/expected/

TESTS_FORMAT = format_01 format_02 format_03

.PHONY: test
test:
	mkdir -p $(OUTPUT_DIR)

	for TEST_NAME in $(TESTS_FORMAT); do \
\
		echo $$TEST_NAME; \
\
		emacs -batch -L $(EMACSLOADPATH) -l pgqa-test.el \
--insert $(INPUT_DIR)/$$TEST_NAME.sql -f pgqa-test-formatting > \
./$(OUTPUT_DIR)/$$TEST_NAME.sql \ || exit; \
\
		diff -s $(OUTPUT_DIR)/$$TEST_NAME.sql \
$(CHECK_DIR)/$$TEST_NAME.sql || exit; \
	done

clean:
	rm -rf $(OUTPUT_DIR)
