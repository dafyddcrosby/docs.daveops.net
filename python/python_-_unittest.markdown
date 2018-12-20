# Python - unittest
@Python 

Up to 2.7, use ``unittest2`` since it's got more assert tests than ``unittest`` (they were all backported from the 2.7 release).

Running tests
-------------



 python -m unittest discover -s project_directory -p '*_test.py'

Example
-------

```python
 import random
 import unittest
 
 class TestSequenceFunctions(unittest.TestCase):
 
 def setUp(self):
 self.seq = range(10)
 
 def test_shuffle(self):
 # make sure the shuffled sequence does not lose any elements
 random.shuffle(self.seq)
 self.seq.sort()
 self.assertEqual(self.seq, range(10))
 
 # should raise an exception for an immutable sequence
 self.assertRaises(TypeError, random.shuffle, (1,2,3))
 
 def test_choice(self):
 element = random.choice(self.seq)
 self.assertTrue(element in self.seq)
 
 def test_sample(self):
 with self.assertRaises(ValueError):
 random.sample(self.seq, 20)
 for element in random.sample(self.seq, 5):
 self.assertTrue(element in self.seq)
 
 if __name__ == '__main__':
 unittest.main()
```
