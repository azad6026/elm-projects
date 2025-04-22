import { readdirSync } from 'fs';
import { execSync } from 'child_process';
import { join, basename } from 'path';

const elmSrcDir = 'src/elm/src';
const elmOutDir = 'public/elm';

// Find all `.elm` files directly under `elm/src/`
const files = readdirSync(elmSrcDir).filter(f => f.endsWith('.elm'));

files.forEach(file => {
  const inputPath = join(elmSrcDir, file);
  const outFile = basename(file, '.elm') + '.js';
  const outputPath = join(elmOutDir, outFile);
  
  console.log(`Compiling ${file} → ${outputPath}`);
  try {
    execSync(`elm make ${inputPath} --output=${outputPath}`, { stdio: 'inherit' });
  } catch (err) {
    console.error(`❌ Failed to compile ${file}`);
  }
});
