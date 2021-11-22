---
title: Ruby - rdoc
---

```text
*word*
    displays word in a bold font
_word_
    displays word in an emphasized font
+word+
    displays word in a code font
```

Comments:

```ruby
# This is the foo function
#--
# Lines between -- and ++ won't be parsed
#++
# It will return true
def foo
  true
end
```

Don't document a thing:

```ruby
module MyModule # :nodoc:
end
```

Links:

```text
https://github.com/example/example
mailto:user@example.com
{RDoc Documentation}[http://rdoc.rubyforge.org]
{RDoc Markup}[rdoc-ref:RDoc::Markup]
```

## Using `ri` on Fedora

To get the system ruby library documentation, you'll need to install `ruby-doc`

```bash
sudo dnf install rubygem-rdoc ruby-doc
```

## Links

- <https://docs.ruby-lang.org/en/2.1.0/RDoc/Markup.html>
- <https://www.mikeperham.com/wp-content/uploads/2010/12/rdoc.html>
- <https://jan.varwig.org/wp-content/uploads/2006/09/Rdoc%20Cheat%20Sheet.pdf>
