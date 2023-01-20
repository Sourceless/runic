use aws_config::meta::region::RegionProviderChain;
use aws_sdk_s3::{Client, Error};
use datafrog::{Iteration, Relation};

#[tokio::main]
async fn main() -> Result<(), Error> {
    let region_provider = RegionProviderChain::default_provider().or_else("us-east-1");
    let config = aws_config::from_env().region(region_provider).load().await;
    let client = Client::new(&config);

    let resp = client.list_buckets().send().await?;

    println!("Tables: ");

    let buckets = resp.buckets().unwrap_or_default();
    let mut bucket_names: Vec<(String, String)> = Vec::new();

    for bucket in buckets {
        let bucket_name = bucket.name().unwrap_or_default();
        bucket_names.push((String::from("James H Avery"), String::from(bucket_name)));
        println!("  {}", bucket_name);
    }

    let mut iteration = Iteration::new();
    let buckets_var = iteration.variable::<(String, String)>("buckets");
    buckets_var.insert(bucket_names.into());

    println!();
    println!("Found {} buckets", buckets.len());
    println!();

    while iteration.changed() {
        // Do nothing -- just needed to mark the variables as stable
    }

    let all_buckets: Relation<(String, String)> = buckets_var.complete();
    println!("{:?}", all_buckets.elements);


    Ok(())
}
