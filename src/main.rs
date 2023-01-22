use aws_config::meta::region::RegionProviderChain;
use aws_sdk_s3::{Client, Error};
use crepe::crepe;
use std::collections::HashMap;

crepe! {
    @input
    struct RemoteResource<'a>(&'a str);
    // change to resource uuids instead!

    @input
    struct DesiredResource<'a>(&'a str);

    @input
    struct Property<'a>(&'a str, &'a str, &'a str);

    @output
    struct ToCreateResource<'a>(&'a str);

    @output
    struct PropertyChanged<'a>(&'a str, &'a str, &'a str, &'a str);

    struct Overlap<'a>(&'a str);
    struct ResourceArn<'a>(&'a str, &'a str);
    struct SameResource<'a>(&'a str, &'a str);

    SameResource(resource1, resource2) <-
        Property(resource1, "arn", value),
        Property(resource2, "arn", value);

    ResourceArn(resource, arn) <-
        Property(resource, "arn", arn);

    // Changed value
    PropertyChanged(arn, key, from_value, to_value) <-
        RemoteResource(remote_id),
        DesiredResource(local_id),
        SameResource(remote_id, local_id),
        ResourceArn(remote_id, arn),
        ResourceArn(local_id, arn),
        Property(remote_id, key, from_value),
        Property(local_id, key, to_value),
        (from_value != to_value);

    PropertyChanged(arn, key, "", to) <-
        ToCreateResource(arn),
        DesiredResource(local_id),
        ResourceArn(local_id, arn),
        Property(local_id, key, to);

    Overlap(arn) <-
        RemoteResource(resource1),
        DesiredResource(resource2),
        (resource1 != resource2),
        SameResource(resource1, resource2),
        ResourceArn(resource1, arn),
        ResourceArn(resource2, arn);

    ToCreateResource(arn) <-
        DesiredResource(local_id),
        ResourceArn(local_id, arn),
        !Overlap(arn);
}

#[derive(Clone)]
struct AWSMockResource<'a> {
    name: &'a str,
    properties: &'a HashMap<&'a str, &'a str>
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    // let region_provider = RegionProviderChain::default_provider().or_else("us-east-1");
    // let config = aws_config::from_env().region(region_provider).load().await;
    // let client = Client::new(&config);

    // let resp = client.list_resources().send().await?;

    // println!("Tables: ");

    // let resources = resp.resources().unwrap_or_default();
    let mut c = Crepe::new();

    let remote_resources = [
        &AWSMockResource { name: &"remote1", properties: &HashMap::from([("mine", "yes"), ("arn", "1")]) },
        &AWSMockResource { name: &"remote2", properties: &HashMap::from([("mine", "yes"), ("arn", "2")]) },
        &AWSMockResource { name: &"remote3", properties: &HashMap::from([("mine", "yes"), ("arn", "3")]) }
    ];

    let desired_resources = [
        &AWSMockResource { name: &"local1", properties: &HashMap::from([("mine", "no"), ("arn", "1")]) },
        &AWSMockResource { name: &"local2", properties: &HashMap::from([("mine", "yes"), ("arn", "2")]) },
        &AWSMockResource { name: &"local3", properties: &HashMap::from([("mine", "yes"), ("arn", "3")]) },
        &AWSMockResource { name: &"local4", properties: &HashMap::from([("mine", "yes"), ("arn", "4")]) }
    ];

    for resource in remote_resources {
        let resource_name = resource.name;
        // let resource_name = resource.name().unwrap_or_default();
        c.extend([RemoteResource(resource_name)]);

        for (key, value) in resource.properties {
            c.extend([Property(resource_name, key, value)])
        }
    }

    for resource in desired_resources {
        let resource_name = resource.name;
        // let resource_name = resource.name().unwrap_or_default();
        c.extend([DesiredResource(resource_name)]);

        for (key, value) in resource.properties {
            c.extend([Property(resource_name, key, value)])
        }
    }

    // println!();
    // println!("Found {} resources", resources.len());
    // println!();

    let (resources_to_create, properties_changed) = c.run();
    let b2cs = resources_to_create.into_iter().collect::<Vec<_>>();
    let pcs = properties_changed.into_iter().collect::<Vec<_>>();

    for resource in b2cs {
        println!("Need to create: {}", resource.0);
    }

    for prop in pcs {
        println!("Changed property {} on resource {}. Old value: {}, new value: {}", prop.1, prop.0, prop.2, prop.3);
    }

    Ok(())
}
