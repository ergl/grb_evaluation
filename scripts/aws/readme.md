# Steps

- `./check_settings.sh <region> <instance-type>=c5.2xlarge`

This checks that we have all the required things for the region, and that there are available machines of that type
This command will output a line you can copy and paste to request spot instances of that type at that region.

- `./request_instances.sh -r <region> -t <type> -i <image> -k <key> -s <sg> -n <amount>`

This will request `amount` machines of `type` in `region`, and output the spot request ids for those machines.

- `./check_instances.sh -r region reseveration_id-1 ....`

This will wait until all the reservation ids at `region` have been fulfilled, and output the id of all the instances
at that region.

You can use `./connect.sh <region> <instance>` to `ssh` into that machine.

---

Now, copy the instance ids into `configuration/cluster.config` (`clusters` key), and fill other details. With this info,
now `automation.escript` can do all the work, just like we did in the apollo cluster.

---

An image can check on http://169.254.169.254/latest/dynamic/instance-identity/document for a json document that includes the following keys:

```json
[
  "accountId",
  "architecture",
  "availabilityZone",
  "billingProducts",
  "devpayProductCodes",
  "marketplaceProductCodes",
  "imageId",
  "instanceId",
  "instanceType",
  "kernelId",
  "pendingTime",
  "privateIp",
  "ramdiskId",
  "region",
  "version",
]
```

More docs at https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/identify_ec2_instances.html and https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-identity-documents.html
