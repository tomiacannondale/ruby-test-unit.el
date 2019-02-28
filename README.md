ruby-test-unit.el --- run Ruby Test::Unit test case in Emacs compilation-mode
=============================================================================

Copyright (C) 2019 by Yoshinori Toki

- Author: Yoshinori Toki <toki@freedom.ne.jp>
- Version: 0.2
- Package-Requires: ((compile) (ruby-mode) (seq) (tramp))
- Keywords: ruby, test
- URL: <https://github.com/y10k/ruby-test-unit.el>

LICENSE
-------
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Commentary
----------
Usage:

modity `.emacs` to set `ruby-test-unit` key definition in `ruby-mode`.

```lisp
(require 'ruby-test-unit)
(add-hook 'ruby-mode-hook
          (lambda () (ruby-test-unit-keys)))
```

Key bindings in `ruby-mode`:

|key binding|function                          |description                             |
|-----------|----------------------------------|----------------------------------------|
|`C-c` `,`  |`ruby-test-unit-run-test-location`|run ruby test at cursor point           |
|`C-c` `.`  |`ruby-test-unit-run-test-method`  |run ruby test method at cursor point    |
|`C-c` `@`  |`ruby-test-unit-run-test-class`   |run ruby test-case class at cursor point|
|`C-c` `f`  |`ruby-test-unit-run-test-file`    |run ruby test file of current buffer    |
|`C-c` `r`  |`ruby-test-unit-run-rake-test`    |run rake test                           |
|`C-c` `c`  |`compile`                         |use to run the last test again          |
