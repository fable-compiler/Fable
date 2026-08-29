// process.env is Node-specific; there is no browser equivalent for reading
// environment variables, so this only works when running under Node.
declare const process: { env: Record<string, string | undefined> };

export function getEnvironmentVariable(name: string): string {
  return process.env[name] as string;
}
