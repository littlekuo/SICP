#lang racket


;

; a = [Ca*(1 - 0.5*Pa), Ca*(1 + 0.5*Pa)]
; b = [Cb*(1 - 0.5*Pb), Cb*(1 + 0.5*Pb)]

; if all endpoints are positive,
;  a*b = [Ca*Cb*(1 - 0.5*(Pa + Pb) + 0.25*Pa*Pb),
;         Ca*Cb*(1 + 0.5*(Pa + Pb) + 0.25*Pa*Pb)]

; ignore the 0.25*Pa*Pb( because it's very small),
;  a*b = [Ca*Cb*(1 - 0.5*(Pa + Pb)), Ca*Cb*(1 + 0.5*(Pa + Pb))]



