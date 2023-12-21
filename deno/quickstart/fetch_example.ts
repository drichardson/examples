const site = await fetch("https://www.deno.com");
console.log(await site.text());
