import crypto from 'crypto';

function generateSecureKey(length: number): string {
  return crypto.randomBytes(length).toString('hex');
}

console.log('=== SECURE KEYS FOR PRODUCTION ===\n');
console.log('ENCRYPTION_KEY=' + generateSecureKey(32));
console.log('JWT_SECRET=' + generateSecureKey(32));
console.log('\nCopy these to your .env file and keep them secure!');
