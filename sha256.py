import hashlib
import struct
import random
import argparse
import sys

k = [0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]
h = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

message_block = []
iv = []
block_header = []


def sha256d(message):
    return hashlib.sha256(hashlib.sha256(message).digest()).digest()


def merkle_root(coinbase1, extra_nonce1, coinbase2, merkle_branch, extra_nonce2):
    coinbase = coinbase1 + extra_nonce1 + extra_nonce2.to_bytes(4, byteorder='big') + coinbase2
    root = sha256d(coinbase)
    for node in merkle_branch:
        root = sha256d(root + node)
    return root


def rotr(x, y):
    return ((x >> y) | (x << (32 - y))) & 0xffffffff


def ssig0(x):
    return rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3)


def ssig1(x):
    return rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10)


def sha256_pipe(state, message_block, n):
    a, b, c, d, e, f, g, h = state
    s0_data = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
    majority = (a & b) ^ (a & c) ^ (b & c)
    t2 = s0_data + majority
    s1_data = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
    choose = (e & f) ^ ((~e) & g)
    t1 = h + s1_data + choose + k[n] + message_block
    h = g
    g = f
    f = e
    e = (d + t1) & 0xffffffff
    d = c
    c = b
    b = a
    a = (t1 + t2) & 0xffffffff
    return [a, b, c, d, e, f, g, h]


def generate_message_block(seed):
    rnd = random.Random()
    rnd.seed(seed)
    message_block = []
    for i in range(16):
        message_block.append(rnd.randrange(2**32))
    for i in range(16, 64):
        message_block.append((message_block[i - 16] + ssig0(message_block[i - 15]) + message_block[i - 7] + ssig1(message_block[i - 2])) & 0xffffffff)
    return message_block


def set_message_block(input_str):
    message_block = list(struct.unpack('>16L', bytes.fromhex(input_str)))
    for i in range(16, 64):
        message_block.append((message_block[i - 16] + ssig0(message_block[i - 15]) + message_block[i - 7] + ssig1(message_block[i - 2])) & 0xffffffff)
    return list(message_block)


def generate_block_header(seed):
    rnd = random.Random()
    rnd.seed(seed)
    block_header = []
    for i in range(19):
        block_header.append(rnd.randrange(2**32))
    return block_header


def set_block_header(input_str):
    return struct.unpack('>19L', bytes.fromhex(input_str))


def set_block_header_from_phantom_stratum(extra_nonce2):
    return set_block_header_from_stratum(
        version=int(0).to_bytes(4, 'big'),
        previous_hash=int(0).to_bytes(32, 'big'),
        coinbase1=int(0).to_bytes(51, byteorder='big'),
        extra_nonce1=int(0).to_bytes(4, byteorder='big'),
        coinbase2=int(0).to_bytes(57, byteorder='big'),
        merkle_branch=[],
        extra_nonce2=extra_nonce2,
        time_stamp=int(0).to_bytes(4, byteorder='big'),
        nbits=int(0).to_bytes(4, byteorder='big'))


def set_block_header_from_stratum(version, previous_hash, coinbase1, extra_nonce1, coinbase2, merkle_branch, extra_nonce2, time_stamp, nbits):
    block_header = version + previous_hash + merkle_root(coinbase1, extra_nonce1, coinbase2, merkle_branch, extra_nonce2) + time_stamp + nbits
    return list(struct.unpack('>19L', block_header))


def get_hash_by_nonce(nonce):
    block_header.append(nonce)
    return sha256d(struct.pack('>20L', *block_header))


def get_core_state_out(nonce):
    message_block.append(nonce)
    message_block.append(0x80000000)
    for i in range(4, 14):
        message_block.append(0)
    message_block.append(0x00000280)
    for i in range(16, 64):
        message_block.append((message_block[i - 16] + ssig0(message_block[i - 15]) + message_block[i - 7] + ssig1(message_block[i - 2])) & 0xffffffff)
    return struct.pack('>8L', *sha256_transform(iv, message_block))


def generate_iv(seed):
    rnd = random.Random()
    rnd.seed(seed)
    iv = []
    for i in range(8):
        iv.append(rnd.randrange(2**32))
    return list(iv)


def set_iv(input_str):
    return list(struct.unpack('>8L', bytes.fromhex(input_str)))


def set_tail(input_str):
    return list(struct.unpack('>3L', bytes.fromhex(input_str)))


def calculate_whk_pre_in(n):
    if(n == 0):
        state_h = iv[7]
    else:
        state_h = sha256_round(iv, message_block, n - 1)[7]
    return (message_block[n] + state_h + k[n]) & 0xffffffff


def calculate_whk_pre_out(n):
    if(n == 63):
        return 0
    else:
        state_h = sha256_round(iv, message_block, n)[7]
    return (message_block[n + 1] + state_h + k[n + 1]) & 0xffffffff


def sha256_round(iv, message_block, n):
    state = iv.copy()
    for i in range(n + 1):
        state = sha256_pipe(state, message_block[i], i)
    return state


def sha256_transform(iv, message_block):
    iv = iv.copy()
    state = sha256_round(iv, message_block, 63)
    return [((x + y) & 0xffffffff) for x, y in zip(iv, state)]


def sha256_core00(block_header):
    message_block = [*struct.unpack('<16L', block_header[:64])]
    block_tail = [*struct.unpack('<3L', block_header[64:76])]
    return sha256_transform(h, message_block) + block_tail


def sha256_core01(mid_state, block_tail):
    message_block = []
    for i in range(3):
        message_block.append(block_tail[i])
    for i in range(3, 31):
        message_block.append(0x80000000)
    message_block.append(0x00000280)
    return sha256_transform(mid_state, message_block)


def sha256_core1(state):
    message_block = []
    for i in range(8):
        message_block.append(state[i])
    message_block.append(0x80000000)
    for i in range(9, 15):
        message_block.append(0x00000000)
    message_block.append(0x000000100)
    return sha256_transform(h, message_block)


def sha256Pipe(input_str):
    (message_block_byte, state_byte, n_byte) = map(lambda x: bytes.fromhex(x), input_str.split(','))
    state = struct.unpack('>8L', state_byte)
    message_block = struct.unpack('>1L', message_block_byte)[0]
    n = struct.unpack('>1L', n_byte)[0]
    return struct.pack('>8L', *sha256_pipe(state, message_block, n))


def sha256Round(input_str):
    return struct.pack('>8L', *sha256_round(iv, message_block, int(input_str)))


def sha256Transform(input_str):
    return struct.pack('>8L', *sha256_transform(iv, message_block))


def sha256Core01(input_str):
    (mid_state_byte, block_tail_byte) = map(lambda x: bytes.fromhex(x), input_str.split(','))
    mid_state = struct.unpack('>8L', mid_state_byte)
    block_tail = struct.unpack('>3L', block_tail_byte)
    return struct.pack('>8L', *sha256_core01(mid_state, block_tail))


def sha256Core1(input_str):
    state_byte = bytes.fromhex(input_str)
    state = struct.unpack('>8L', state_byte)
    return struct.pack('>8L', *sha256_core1(state))


def getMessageBlock(input_str):
    return struct.pack('>64L', *message_block)


def getIV(input_str):
    return struct.pack('>8L', *iv)

def getDataTailForASICBoost(input_str):
    return struct.pack('>3L', *message_block[:3])

def getMidState(input_str):
    message_block = block_header[:16]
    for i in range(16, 64):
        message_block.append((message_block[i - 16] + ssig0(message_block[i - 15]) + message_block[i - 7] + ssig1(message_block[i - 2])) & 0xffffffff)
    return struct.pack('>8L', *sha256_transform(h, message_block))


def getDataTail(input_str):
    return struct.pack('>3L', *block_header[16:19])


def getHashByNonce(input_str):
    return get_hash_by_nonce(int(input_str))

def getCoreStateOut(input_str):
    return get_core_state_out(int(input_str))


def getWHKPreIn(input_str):
    return struct.pack('>1L', calculate_whk_pre_in(int(input_str)))


def getWHKPreOut(input_str):
    return struct.pack('>1L', calculate_whk_pre_out(int(input_str)))


if __name__ == '__main__':
    sha256_function = {
        "getMessageBlock": getMessageBlock,
        "getIV": getIV,
        "getWHKPreIn": getWHKPreIn,
        "getWHKPreOut": getWHKPreOut,
        "sha256Round": sha256Round,
        "sha256Transform": sha256Transform,
        "getCoreStateOut": getCoreStateOut,
        "getDataTailForASICBoost": getDataTailForASICBoost
    }
    sha256d_function = {
        "getMidState": getMidState,
        "getDataTail": getDataTail,
        "getHashByNonce": getHashByNonce
    }
    parser = argparse.ArgumentParser(description="ASIC sha256d chip test model.")
    parser.add_argument("--iv", type=str, help="set iv in hex.")
    parser.add_argument("--tail", type=str, help="set iv in hex.")
    parser.add_argument("--iv_seed", type=int, help="give a random seed to generate random iv.")
    parser.add_argument("--tail_seed", type=str, help="give a random seed to generate random tail.")
    parser.add_argument("--message_block", type=str, help="set message block in hex.")
    parser.add_argument("--block_header_seed", type=int, help="give a random seed to generate random block_header.")
    parser.add_argument("--block_header", type=str, help="set block header in hex.")
    parser.add_argument("--message_block_seed", type=int, help="give a random seed to generate random message_block.")
    parser.add_argument("--extra_nonce2", type=int, help="use extra_nonce and all 0 stratum work to generate a new block header.")
    parser.add_argument("--target_function", required=True, type=str, help="target function to test.")
    parser.add_argument("--target_function_arg", type=str, help="arg pass to target function.")
    args = parser.parse_args()
    if args.target_function in sha256_function.keys():
        if args.iv is not None:
            iv = set_iv(args.iv)
        elif args.iv_seed is not None:
            iv = generate_iv(args.iv_seed)
        else:
            raise Exception("calling " + str(sys.argv) + "failed")
        if args.message_block is not None:
            message_block = set_message_block(args.message_block)
        elif args.message_block_seed is not None:
            message_block = generate_message_block(args.message_block_seed)
        elif args.tail is not None:
            message_block = set_tail(args.tail)
        elif args.tail_seed is not None:
            message_block = generate_message_block(args.tail_seed)[:3]
        else:
            raise Exception("calling " + str(sys.argv) + "failed")
        print(sha256_function[args.target_function](args.target_function_arg).hex())
    if args.target_function in sha256d_function.keys():
        if args.block_header is not None:
            block_header = set_block_header(args.block_header)
        elif args.block_header_seed is not None:
            block_header = generate_block_header(args.block_header_seed)
        elif args.extra_nonce2 is not None:
            block_header = set_block_header_from_phantom_stratum(args.extra_nonce2)
        print(sha256d_function[args.target_function](args.target_function_arg).hex())
