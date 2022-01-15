# Phusion Passenger


# Phusion Passenger

CLI


```bash
# Get overall status
passenger-status
# current requests
passenger-status --show=requests
# restart app
passenger-config restart-app
```

## Hiding Version Headers



  # Apache
  LoadModule headers_module modules/mod_headers.so # if not already loaded
  Header always unset "X-Powered-By"
  Header always unset "X-Runtime"
  # nginx (in http context)
  passenger_show_version_in_header off;

## Resources

<https://www.phusionpassenger.com/library/admin/apache/>

