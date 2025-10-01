#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  9 00:19:16 2024

@author: joao
"""

import torch
import esm
import random
import math
import argparse

# Load ESM-2 model
#model, alphabet = esm.pretrained.load_model_and_alphabet_local("/home/joao.sartori/proj.joao.sartori/ESM2/checkpoints/esm2_t33_650M_UR50D.pt")
model, alphabet = esm.pretrained.esm2_t33_650M_UR50D()
batch_converter = alphabet.get_batch_converter()

def insert_mask(sequence, position, mask="<mask>"):
    """
    Replaces a character in a given position of a sequence with a mask.

    Parameters:
    - sequence (str or list): The sequence to replace the character in.
    - position (int): The position in the sequence where the character should be replaced.
    - mask (str): The mask to insert (default is "<mask>").

    Returns:
    - str or list: The sequence with the mask replacing the character at the specified position.
    """

    if not (0 <= position < len(sequence)):
        raise ValueError("Position is out of bounds.")

    if isinstance(sequence, str):
        return sequence[:position] + mask + sequence[position + 1:]
    elif isinstance(sequence, list):
        return sequence[:position] + [mask] + sequence[position + 1:]
    else:
        raise TypeError("Sequence must be a string or list.")

def complete_mask(input_sequence, posi, temperature=1.0):

    data = [
        ("protein1", insert_mask(input_sequence, posi, mask="<mask>"))
    ]

    batch_labels, batch_strs, batch_tokens = batch_converter(data)
    batch_lens = (batch_tokens != alphabet.padding_idx).sum(1)

    # Predict masked tokens
    with torch.no_grad():
        token_probs = model(batch_tokens, repr_layers=[33])["logits"]

    # Apply temperature
    token_probs /= temperature

    softmax = torch.nn.Softmax(dim=-1)
    probabilities = softmax(token_probs)

    # Get the index of the <mask> token
    mask_idx = (batch_tokens == alphabet.mask_idx).nonzero(as_tuple=True)

    # Extract the predicted tokens for the masked positions
    predicted_tokens = torch.argmax(probabilities, dim=-1)

    # Sample from the probability distribution
    predicted_tokens = torch.multinomial(probabilities[mask_idx], num_samples=1).squeeze(-1)

    # Replace the <mask> token with the predicted token
    batch_tokens[mask_idx] = predicted_tokens


    predicted_residues = [alphabet.get_tok(pred.item()) for pred in batch_tokens[0]]

    seq_predicted = ''.join(predicted_residues[1:-1])


    if input_sequence != seq_predicted:
        print("Mutation added!! 😉")

    return seq_predicted

parser = argparse.ArgumentParser(description="Process and complete protein sequences with masked tokens.")
parser.add_argument("--sequence", type=str, help="Protein sequence")
parser.add_argument("--position", type=int, help="Position to mask")
parser.add_argument("--temperature", type=float, default=1.0, help="Temperature for scaling probabilities")
args = parser.parse_args()

completed_sequence = complete_mask(args.sequence, args.position, args.temperature)

# with open("completed_sequence.txt", "w") as f:
#         f.write(completed_sequence)

with open("completed_sequence.txt", "w") as f:
    f.write(completed_sequence)
    f.close()
