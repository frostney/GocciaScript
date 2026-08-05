export let counter = 0;

export function bump() {
  counter += 1;
}

const secret = "s3cret";

export default function readSecret() {
  return secret;
}
