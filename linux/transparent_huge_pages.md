---
title: Linux/Transparent Huge Pages
---

## Centos

```bash
cat /sys/kernel/mm/transparent_hugepage/enabled
cat /sys/kernel/mm/transparent_hugepage/defrag
```

To disable:

```bash
echo 'never' > /sys/kernel/mm/transparent_hugepage/enabled
echo 'never' > /sys/kernel/mm/transparent_hugepage/defrag
```

## Links

* [THP kernel doc](https://www.kernel.org/doc/Documentation/vm/transhuge.txt)
* [hugetlbpage kernel doc](https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt)
