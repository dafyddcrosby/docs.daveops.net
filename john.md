# john

```bash
# Convert raw packet capture
wpacap2john cap.raw > cap.john
john -form wpapsk cap.john
```
