"use strict";

const audioContext = new (window.AudioContext || window.webkitAudioContext)();

/**
 * @type {Map<string, { source: AudioBufferSourceNode, gainNode: GainNode }>}
 */
const nodesMap = new Map();

export function registerNodes(id) {
  const nodes = {
    source: audioContext.createBufferSource(),
    gainNode: audioContext.createGain(),
  };
  nodesMap.set(id, nodes);
}

export function deleteNodes(id) {
  nodesMap.delete(id);
}

export function playAudioImpl(
  id,
  buffer,
  offset,
  duration,
  fadeIn,
  fadeOut,
  loopOpts,
) {
  return function () {
    const element = nodesMap.get(id);
    if (element === undefined) {
      return { tag: "Left", value: id + " not found" };
    }

    const { source, gainNode } = element;

    audioContext.decodeAudioData(buffer, decodedData => {
      source.buffer = decodedData;

      if (loopOpts) {
        source.loop = true;
        source.loopStart = loopOpts.start;
        source.loopEnd = loopOpts.end;
      }

      gainNode.gain.setValueAtTime(0, audioContext.currentTime);
      gainNode.gain.linearRampToValueAtTime(1, audioContext.currentTime + fadeIn / 1000);

      source.connect(gainNode);
      gainNode.connect(audioContext.destination);

      source.start(audioContext.currentTime + offset / 1000, 0, duration / 1000);

      // Handle fadeOut
      gainNode.gain.setValueAtTime(1, audioContext.currentTime + duration / 1000 - fadeOut / 1000);
      gainNode.gain.linearRampToValueAtTime(0, audioContext.currentTime + duration / 1000);
    });

    return { tag: "Right", value: null };
  };
}

export function stopAudioImpl(id, fadeOut) {
  return function () {
    const element = nodesMap.get(id);
    if (element === undefined) {
      return { tag: "Left", value: id + " not found" };
    }

    const { source, gainNode } = element;

    gainNode.gain.cancelScheduledValues(audioContext.currentTime);
    gainNode.gain.setValueAtTime(gainNode.gain.value, audioContext.currentTime);
    gainNode.gain.linearRampToValueAtTime(0, audioContext.currentTime + fadeOut / 1000);

    setTimeout(() => {
      source.stop();
    }, fadeOut);

    return { tag: "Right", value: null };
  };
}

export function changeVolumeImpl(id, volume) {
  return function () {
    const element = nodesMap.get(id);
    if (element === undefined) {
      return { tag: "Left", value: id + " not found" };
    }

    const { gainNode } = element;

    gainNode.gain.setValueAtTime(volume, audioContext.currentTime);

    return { tag: "Right", value: null };
  };
}
