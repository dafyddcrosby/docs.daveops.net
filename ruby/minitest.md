# Minitest

- <http://docs.seattlerb.org/minitest/>


## Mocking methods

```ruby
# method mocking is done with a block
ClassName.stub :method_name, method_value do
  ClassName.method_to_run_against
end
```