"use strict";

import { Left, Right } from '#/Data.Either';
import { Just } from '#/Data.Maybe'

const audioContext = new (window.AudioContext || window.webkitAudioContext)();

/**
 * @type {Map<string, { source: AudioBufferSourceNode, gainNode: GainNode }>}
 */
const nodesMap = new Map();

export function registerNodes(id) {
  return function () {
    if (nodesMap.has(id)) {
      return Left.create('Node already exists');
    }
    const nodes = {
      source: audioContext.createBufferSource(),
      gainNode: audioContext.createGain(),
    };
    nodesMap.set(id, nodes);
    return Right.create(undefined);
  }
}

export function deleteNodes(id) {
  return function () {
    nodesMap.delete(id);
  }
}

export function playAudioImpl(
  id,
  buffer,
  delayMs,
  offsetMs,
  fadeIn,
  fadeOut,
  loopOpts,
) {
  return function () {
    const element = nodesMap.get(id);
    if (element === undefined) {
      return Left.create(`Node ${id} not found`);
    }

    const { source, gainNode } = element;

    audioContext.decodeAudioData(buffer, decodedData => {
      const sampleRate = decodedData.sampleRate;
      source.buffer = decodedData;

      if (loopOpts instanceof Just) {
        const { start, end } = loopOpts.value0
        source.loop = true;
        source.loopStart = start / sampleRate;
        source.loopEnd = end / sampleRate;
      }

      gainNode.gain.setValueAtTime(0, audioContext.currentTime);
      gainNode.gain.linearRampToValueAtTime(1, audioContext.currentTime + fadeIn / 1000);

      source.connect(gainNode);
      gainNode.connect(audioContext.destination);

      source.start(delayMs / 1000, offsetMs / 1000);

      // Handle fadeOut
      if (!source.loop) {
        gainNode.gain.setValueAtTime(1, decodedData.duration - fadeOut / 1000);
        gainNode.gain.linearRampToValueAtTime(0, decodedData.duration);
      }
    });

    return Right.create(undefined);
  };
}

export function stopAudioImpl(id, fadeOut) {
  return function () {
    const element = nodesMap.get(id);
    if (element === undefined) {
      return Left.create(`Node ${id} not found`);
    }

    const { source, gainNode } = element;

    gainNode.gain.cancelScheduledValues(audioContext.currentTime);
    gainNode.gain.setValueAtTime(gainNode.gain.value, audioContext.currentTime);
    gainNode.gain.linearRampToValueAtTime(0, audioContext.currentTime + fadeOut / 1000);

    setTimeout(() => {
      source.stop();
    }, fadeOut);

    return Right.create(undefined);
  };
}

export function changeVolumeImpl(id, volume) {
  return function () {
    const element = nodesMap.get(id);
    if (element === undefined) {
      return Left.create(`Node ${id} not found`);
    }

    const { gainNode } = element;

    gainNode.gain.setValueAtTime(volume, audioContext.currentTime);

    return Right.create(undefined);
  };
}
