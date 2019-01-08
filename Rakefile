# -*- coding: utf-8 -*-

require 'rake/clean'

desc 'converto README markdown to html'
task :readme => [ 'README.html' ]

file 'README.html' => [ 'README.md' ] do
  sh "pandoc --from=markdown --to=html5 --standalone --self-contained --css=$HOME/.pandoc/github.css --output=README.html README.md"
end
CLOBBER.include 'README.html'

# Local Variables:
# mode: Ruby
# indent-tabs-mode: nil
# End:
