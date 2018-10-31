#include <stdint.h>
#include <stdio.h>
#include <memory.h>

#define SHA256_BLOCK_SIZE 64

struct sha256_ctx
{
	uint32_t sz;
	uint8_t  buf[128];
	uint32_t h[8];
};

#define SHA256_DIGEST_SIZE	32
#define SHA256_CTX_SIZE		sizeof(struct sha256_ctx)

void cryptonite_sha256_init(struct sha256_ctx *ctx);
void cryptonite_sha256_update(struct sha256_ctx *ctx, const uint8_t *data, uint32_t len);
void cryptonite_sha256_finalize(struct sha256_ctx *ctx, uint8_t *out);


void cryptonite_sha256_init(struct sha256_ctx *ctx)
{
	memset(ctx, 0, sizeof(*ctx));

	ctx->h[0] = 0x6a09e667;
	ctx->h[1] = 0xbb67ae85;
	ctx->h[2] = 0x3c6ef372;
	ctx->h[3] = 0xa54ff53a;
	ctx->h[4] = 0x510e527f;
	ctx->h[5] = 0x9b05688c;
	ctx->h[6] = 0x1f83d9ab;
	ctx->h[7] = 0x5be0cd19;
}

/* 232 times the cube root of the first 64 primes 2..311 */
static const uint32_t k[] = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
	0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
	0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
	0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
	0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
	0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

static inline uint32_t ror32(uint32_t word, uint32_t shift)
{
	return (word >> shift) | (word << (32 - shift));
}

#define e0(x)       (ror32(x, 2) ^ ror32(x,13) ^ ror32(x,22))
#define e1(x)       (ror32(x, 6) ^ ror32(x,11) ^ ror32(x,25))
#define s0(x)       (ror32(x, 7) ^ ror32(x,18) ^ (x >> 3))
#define s1(x)       (ror32(x,17) ^ ror32(x,19) ^ (x >> 10))

# define cpu_to_be64(a) bitfn_swap64(a)
# define be32_to_cpu_array(d, s, l) array_swap32(d, s, l)
# define cpu_to_be32_array(d, s, l) array_swap32(d, s, l)
static inline void store_be32(uint8_t *dst, const uint32_t v)
{
	dst[3] = v; dst[2] = v >> 8; dst[1] = v >> 16; dst[0] = v >> 24;
}

static inline uint32_t bitfn_swap32(uint32_t a)
{
	return (a << 24) | ((a & 0xff00) << 8) | ((a >> 8) & 0xff00) | (a >> 24);
}
static inline void array_swap32(uint32_t *d, uint32_t *s, uint32_t nb)
{
	while (nb--)
		*d++ = bitfn_swap32(*s++);
}

static inline uint64_t bitfn_swap64(uint64_t a)
{
	return ((uint64_t) bitfn_swap32((uint32_t) (a >> 32))) |
                (((uint64_t) bitfn_swap32((uint32_t) a)) << 32);
}

static void sha256_do_chunk(struct sha256_ctx *ctx, uint32_t buf[])
{
	uint32_t a, b, c, d, e, f, g, h, t1, t2;
	int i;
	uint32_t w[64];
        for(i=0;i<64;i++)
                w[i] = 0;
        
	cpu_to_be32_array(w, buf, 16);
        for(i=0;i<16;i++)
                printf("%08x,",w[i]);
        printf("\n");
        
	for (i = 16; i < 64; i++)
		w[i] = s1(w[i - 2]) + w[i - 7] + s0(w[i - 15]) + w[i - 16];
        for(i=0;i<64;i++)
                printf("%08x,",w[i]);
        printf("\n");

	a = ctx->h[0]; b = ctx->h[1]; c = ctx->h[2]; d = ctx->h[3];
	e = ctx->h[4]; f = ctx->h[5]; g = ctx->h[6]; h = ctx->h[7];

#define R(a, b, c, d, e, f, g, h, k, w)                 \
	t1 = h + e1(e) + (g ^ (e & (f ^ g))) + k + w; 	\
	t2 = e0(a) + ((a & b) | (c & (a | b)));         \
	d += t1;					\
	h = t1 + t2;

	for (i = 0; i < 64; i += 8) {
		R(a, b, c, d, e, f, g, h, k[i + 0], w[i + 0]);
		R(h, a, b, c, d, e, f, g, k[i + 1], w[i + 1]);
		R(g, h, a, b, c, d, e, f, k[i + 2], w[i + 2]);
		R(f, g, h, a, b, c, d, e, k[i + 3], w[i + 3]);
		R(e, f, g, h, a, b, c, d, k[i + 4], w[i + 4]);
		R(d, e, f, g, h, a, b, c, k[i + 5], w[i + 5]);
		R(c, d, e, f, g, h, a, b, k[i + 6], w[i + 6]);
		R(b, c, d, e, f, g, h, a, k[i + 7], w[i + 7]);
	}

#undef R

	ctx->h[0] += a; ctx->h[1] += b; ctx->h[2] += c; ctx->h[3] += d;
	ctx->h[4] += e; ctx->h[5] += f; ctx->h[6] += g; ctx->h[7] += h;
        for(i=0;i<8;i++)
                printf("%08x,",ctx->h[i]);
        printf("\n");
}


void cryptonite_sha256_update(struct sha256_ctx *ctx, const uint8_t *data, uint32_t len)
{
	uint32_t index, to_fill;

	/* check for partial buffer */
	index = ctx->sz & 0x3f;
	to_fill = 64 - index;

	ctx->sz += len;

	/* process partial buffer if there's enough data to make a block */
	if (index && len >= to_fill) {
		memcpy(ctx->buf + index, data, to_fill);
		sha256_do_chunk(ctx, (uint32_t *) ctx->buf);
		len -= to_fill;
		data += to_fill;
		index = 0;
	}

	{
		/* process as much 64-block as possible */
		for (; len >= 64; len -= 64, data += 64)
			sha256_do_chunk(ctx, (uint32_t *) data);
	}

	/* append data into buf */
	if (len)
		memcpy(ctx->buf + index, data, len);
}


void cryptonite_sha256_finalize(struct sha256_ctx *ctx, uint8_t *out)
{
	static uint8_t padding[64] = { 0x80, };
	uint64_t bits;
	uint32_t i, index, padlen;

	/* cpu -> big endian */
	bits = cpu_to_be64(ctx->sz << 3);

	/* pad out to 56 */
	index = ctx->sz & 0x3f;
	padlen = (index < 56) ? (56 - index) : ((64 + 56) - index);
	cryptonite_sha256_update(ctx, padding, padlen);

	/* append length */
	cryptonite_sha256_update(ctx, (uint8_t *) &bits, sizeof(bits));

	/* store to digest */
	for (i = 0; i < 8; i++)
		store_be32(out+4*i, ctx->h[i]);
}


int
main(){
        struct sha256_ctx ctx;
        uint8_t data[]="helloworld";
        uint8_t out[64];
        int i;
        cryptonite_sha256_init(&ctx);
        cryptonite_sha256_update(&ctx, data, sizeof(data)-1);
        cryptonite_sha256_finalize(&ctx,out);
        //output = 936a185caaa266bb9cbe981e9e05cb78cd732b0b3280eb944412bb6f8f8f07af
        for(i=0;i<32;i++)
                printf("%02x",out[i]);
        printf("\n");
        return 0;
}
