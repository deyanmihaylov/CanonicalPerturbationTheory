(* ::Package:: *)

(* ::Title:: *)
(*KerrOrbitalParameters Package*)


(* ::Input::Initialization:: *)
BeginPackage["KerrOrbitalParameters`"];


(*
Assembled by Maarten van de Meent (mmeent@aei.mpg.de)
See inline credits for appropriate sources.
*)


(* ::Section:: *)
(*Preamble*)


(* ::Input::Initialization:: *)
KerrEnergy::usage="KerrEnergy[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) Energy of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrAngularMomentum::usage="KerrAngularMomentum[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 2) angular momentum of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrCarterConstant::usage="KerrCarterConstant[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 4) Carter constant of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrRadialRoot::usage="KerrRadialRoot[a,\[Rho]max,\[Rho]min,zmax,n] gives the dimensionless (mass dimension 1) radial roots of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a. Ordered from large to small.";
KerrPolarRoot::usage="KerrPolarRoot[a,\[Rho]max,\[Rho]min,zmax,n] gives the dimensionless (mass dimension 0) polar roots of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a. Ordered from small to large.";
MinoRadialFrequency::usage="MinoRadialFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) Mino time radial frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoPolarFrequency::usage="MinoPolarFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) Mino time polar frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoAzimuthalFrequency::usage="MinoAzimuthalFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) Mino time azimuthal frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoAzimuthalFrequencyr::usage="MinoAzimuthalFrequencyr[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) radial part of the Mino time azimuthal frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoAzimuthalFrequency\[Theta]::usage="MinoAzimuthalFrequency\[Theta][a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) polar part of the Mino time azimuthal frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoFrequencies::usage="MinoFrequencies[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 1) Mino time frequencies {\!\(\*SubscriptBox[\(\[CapitalUpsilon]\), \(r\)]\),\!\(\*SubscriptBox[\(\[CapitalUpsilon]\), \(\[Theta]\)]\),\!\(\*SubscriptBox[\(\[CapitalUpsilon]\), \(\[Phi]\)]\)} of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoGamma::usage="MinoGamma[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 2) average time advance of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoGammar::usage="MinoGammar[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 2) radial part of the average time advance of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
MinoGamma\[Theta]::usage="MinoGammar[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 2) radial part of the average time advance of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrRadialFrequency::usage="KerrRadialFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) asymptotic observer radial frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrPolarFrequency::usage="KerrPolarFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) asymptotic observer polar frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrAzimuthalFrequency::usage="KerrAzimuthalFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) asymptotic observer azimuthal frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrFrequencies::usage="KerrFrequencies[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) asymptotic observer frequencies {\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(r\)]\),\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(\[Theta]\)]\),\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(\[Phi]\)]\)} of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
ProperFrequencies::usage="ProperFrequencies[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) proper time frequencies {\!\(\*SubscriptBox[\(\[Omega]\), \(r\)]\),\!\(\*SubscriptBox[\(\[Omega]\), \(\[Theta]\)]\),\!\(\*SubscriptBox[\(\[Omega]\), \(\[Phi]\)]\)} of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
ProperRadialFrequency::usage="ProperRadialFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) proper time radial frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
ProperPolarFrequency::usage="ProperPolarFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) proper time polar frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
ProperAzimuthalFrequency::usage="ProperAzimuthalFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension -1) proper time azimuthal frequency of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrProperFactor::usage="KerrProperFactor[a,\[Rho]max,\[Rho]min,zmax] gives the dimensionless (mass dimension 2) ratio \[CapitalUpsilon]/\[Omega] between Mino/proper time frequencies of a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";

MinoPrecessionFrequency::usage="MinoPrecessionFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the orbit averaged (Marck) precession frequency w.r.t. Mino time along a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
KerrPrecessionFrequency::usage="KerrPrecessionFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the orbit averaged (Marck) precession frequency w.r.t. Kerr coordinate time along a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";
ProperPrecessionFrequency::usage="ProperPrecessionFrequency[a,\[Rho]max,\[Rho]min,zmax] gives the orbit averaged (Marck) precession frequency w.r.t. proper time along a Kerr orbit with radial turning points \[Rho]max and \[Rho]min, and polar turning point zmax, around a black hole with dimensionless spin a.";


(* ::Section:: *)
(*Private*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(*elK[k_?NumericQ]:=(ClearSystemCache[];EllipticK[k]) (*Workaround for bug in EllipticK*)*)


(* ::Input::Initialization:: *)
elK=EllipticK;


(* ::Input::Initialization:: *)
\[Gamma][T_]:=1-T^2
\[CapitalDelta][\[Rho]_,a_]:= \[Rho]^2-2\[Rho]+a^2
d[a_,\[Rho]_,zm_]:= \[CapitalDelta][\[Rho],a](\[Rho]^2+zm^2 a^2);
f[a_,\[Rho]_,zm_]:=\[Rho]^4+a^2 (\[Rho](\[Rho]+2)+zm^2 \[CapitalDelta][\[Rho],a]);
g[a_,\[Rho]_,zm_]:=2 a \[Rho];
h[a_,\[Rho]_,zm_]:=\[Rho](\[Rho]-2)+(zm^2 \[CapitalDelta][\[Rho],a])/(1-zm^2); 


(* ::Subsection::Closed:: *)
(*Energy*)


(*From App A of  Drasco& Hughes Phys.Rev.D73 (2006) no.2,024027*)


(* ::Input::Initialization:: *)
KerrEnergy[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[
{\[Kappa]=d[a,\[Rho]2,zm]h[a,\[Rho]1,zm]-d[a,\[Rho]1,zm]h[a,\[Rho]2,zm],
\[Epsilon]=d[a,\[Rho]2,zm]g[a,\[Rho]1,zm]-d[a,\[Rho]1,zm]g[a,\[Rho]2,zm],
\[Rho]=f[a,\[Rho]2,zm]h[a,\[Rho]1,zm]-f[a,\[Rho]1,zm]h[a,\[Rho]2,zm],
\[Eta]=f[a,\[Rho]2,zm]g[a,\[Rho]1,zm]-f[a,\[Rho]1,zm]g[a,\[Rho]2,zm],
\[Sigma]=g[a,\[Rho]2,zm]h[a,\[Rho]1,zm]-g[a,\[Rho]1,zm]h[a,\[Rho]2,zm]},
Sqrt[ ((\[Kappa] \[Rho]+2\[Epsilon] \[Sigma] -2 a Sqrt[\[Sigma] (\[Sigma] \[Epsilon]^2+\[Rho] \[Epsilon] \[Kappa]-\[Eta] \[Kappa]^2)/a^2])/(\[Rho]^2+4\[Eta] \[Sigma]))]]


(* ::Input::Initialization:: *)
KerrEnergy[a_,\[Rho]1_,\[Rho]1_,zm_]:=\[Sqrt](((-3+\[Rho]1) (-2+\[Rho]1)^2 \[Rho]1^5+a^6 zm^4 (-1+zm^2 (-1+\[Rho]1)^2+3 \[Rho]1)+a^4 zm^2 \[Rho]1^2 (-4+zm^2 \[Rho]1 (-5+3 \[Rho]1))+a^2 \[Rho]1^3 ((5-3 \[Rho]1) \[Rho]1+zm^2 (4+(-1+\[Rho]1) \[Rho]1 (-7+3 \[Rho]1)))-2 a (1-zm^2)^(1/2) Sqrt[\[Rho]1] (\[Rho]1^2-a^2 zm^2)^(3/2) (a^2+(-2+\[Rho]1) \[Rho]1))/((a^2 zm^2+\[Rho]1^2) ((-3+\[Rho]1)^2 \[Rho]1^4+a^4 (zm^4 (-1+\[Rho]1)^2+4 zm^2 \[Rho]1)+2 a^2 \[Rho]1^2 (-2 \[Rho]1+zm^2 (-3+\[Rho]1^2)))));


(* ::Input::Initialization:: *)
KerrEnergy[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=1;


(* ::Input::Initialization:: *)
KerrEnergy[0|_?PossibleZeroQ,\[Rho]1_,\[Rho]2_,zm_]:=Sqrt[((-2+\[Rho]1) (-2+\[Rho]2) (\[Rho]1+\[Rho]2))/(\[Rho]1^2 (-2+\[Rho]2)+\[Rho]1 (-2+\[Rho]2) \[Rho]2-2 \[Rho]2^2)];


(* ::Input::Initialization:: *)
KerrEnergy[a_,\[Rho]1_,\[Rho]2_,1|1.|_Real?(N@#===1.&)]:=Sqrt[((a^2+(-2+\[Rho]1) \[Rho]1) (\[Rho]1+\[Rho]2) (a^2+(-2+\[Rho]2) \[Rho]2))/(a^4 (2+\[Rho]1+\[Rho]2)+\[Rho]1 \[Rho]2 (\[Rho]1^2 (-2+\[Rho]2)+\[Rho]1 (-2+\[Rho]2) \[Rho]2-2 \[Rho]2^2)+a^2 (\[Rho]1^3+\[Rho]1^2 \[Rho]2+\[Rho]1 (-4+\[Rho]2) \[Rho]2+\[Rho]2^3))];


(* ::Subsection::Closed:: *)
(*Angular Momentum*)


(*From App A of  Drasco& Hughes Phys.Rev.D73 (2006) no.2,024027*)


(* ::Input::Initialization:: *)
KerrAngularMomentum[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{
d2= d[a,\[Rho]2,zm],
f2= f[a,\[Rho]2,zm],
g2= g[a,\[Rho]2,zm],
h2= h[a,\[Rho]2,zm],
T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm]},
(  -((g2 T)/h2)+1/h2 Sqrt[(g2^2+h2 f2 )T^2-h2 d2])];


(* ::Input::Initialization:: *)
KerrAngularMomentum[a_,\[Rho]1_,\[Rho]2_,1|1.|_Real?(N@#===1.&)]:=0;


(* ::Input::Initialization:: *)
KerrAngularMomentum[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=((1-zm^2) (-2 a \[Rho]2+Sqrt[2] Sqrt[-((\[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2))/(-1+zm^2))]))/(a^2 zm^2+(-2+\[Rho]2) \[Rho]2);


(* ::Subsection::Closed:: *)
(*Carter Constant*)


(*From App A of  Drasco& Hughes Phys.Rev.D73 (2006) no.2,024027*)


(* ::Input::Initialization:: *)
KerrCarterConstant[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm]},(  zm^2 ( a^2 \[Gamma][T]+L^2/(1-zm^2)))];


(* ::Input::Initialization:: *)
KerrCarterConstant[a_,\[Rho]1_,\[Rho]2_,1|1.|_Real?(N@#===1.&)]:=(2 \[Rho]1 \[Rho]2 (a^4+\[Rho]1^2 \[Rho]2^2+a^2 (-2 \[Rho]1+\[Rho]1^2+(-2+\[Rho]2) \[Rho]2)))/(a^4 (2+\[Rho]1+\[Rho]2)+\[Rho]1 \[Rho]2 (\[Rho]1^2 (-2+\[Rho]2)+\[Rho]1 (-2+\[Rho]2) \[Rho]2-2 \[Rho]2^2)+a^2 (\[Rho]1^3+\[Rho]1^2 \[Rho]2+\[Rho]1 (-4+\[Rho]2) \[Rho]2+\[Rho]2^3));


(* ::Input::Initialization:: *)
KerrCarterConstant[a_,Infinity|ComplexInfinity,\[Rho]2_,1|1.|_Real?(N@#===1.&)]:=(2 \[Rho]2 (a^2+\[Rho]2^2))/(a^2+(-2+\[Rho]2) \[Rho]2);


(* ::Subsection::Closed:: *)
(*Roots of the Radial equation*)


(*from Fujita& Hikida, Class.Quant.Grav.26 (2009) 135002*)


(* ::Input::Initialization:: *)
KerrRadialRoot[a_,\[Rho]1_,\[Rho]2_,zm_,1]:=\[Rho]1;


(* ::Input::Initialization:: *)
KerrRadialRoot[a_,\[Rho]1_,\[Rho]2_,zm_,2]:=\[Rho]2;


(* ::Input::Initialization:: *)
KerrRadialRoot[a_,\[Rho]1_,\[Rho]2_,zm_,3]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],Q=KerrCarterConstant[a,\[Rho]1,\[Rho]2,zm]},
(2-\[Rho]1 \[Gamma][T]-\[Rho]2 \[Gamma][T]+Sqrt[-((4 a^2 Q \[Gamma][T])/(\[Rho]1 \[Rho]2))+(-2+(\[Rho]1+\[Rho]2) \[Gamma][T])^2])/(2 \[Gamma][T])
];


(* ::Input::Initialization:: *)
KerrRadialRoot[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_,3]:=1/4 (1-2 \[Rho]2+1/(a^2 zm^2+(-2+\[Rho]2) \[Rho]2)^2 (-a^4 (zm^4-2 zm^2 \[Rho]2)-4 Sqrt[2] a \[Rho]2 Sqrt[(1-zm^2) \[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2)]+(-2+\[Rho]2) \[Rho]2^2 (2+\[Rho]2 (-1+2 \[Rho]2))+2 a^2 \[Rho]2 (\[Rho]2 (2+\[Rho]2)+zm^2 (2+(-5+\[Rho]2) \[Rho]2)))+\[Sqrt]((-8 a^2 zm^2  (-2 a \[Rho]2 Sqrt[1-zm^2]+Sqrt[2] Sqrt[\[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2)])^2)/(\[Rho]2 (a^2 zm^2+(-2+\[Rho]2) \[Rho]2)^2)+(4 \[Rho]2^2 (a^4 zm^2 (-1+zm^2)-2 (-2+\[Rho]2) \[Rho]2^2+a^2 \[Rho]2 (-2+(-1+zm^2) \[Rho]2)+2 Sqrt[2] a Sqrt[(1-zm^2) \[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2)])^2)/(a^2 zm^2+(-2+\[Rho]2) \[Rho]2)^4));


(* ::Input::Initialization:: *)
KerrRadialRoot[a_,\[Rho]1_,\[Rho]2_,zm_,4]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],Q=KerrCarterConstant[a,\[Rho]1,\[Rho]2,zm]},
(2 a^2 Q)/(\[Rho]1 \[Rho]2 (2-\[Rho]1 \[Gamma][T]-\[Rho]2 \[Gamma][T]+Sqrt[-((4 a^2 Q \[Gamma][T])/(\[Rho]1 \[Rho]2))+(-2+(\[Rho]1+\[Rho]2) \[Gamma][T])^2]))
];


(* ::Input::Initialization:: *)
KerrRadialRoot[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_,4]:=1/4 (1-2 \[Rho]2+1/(a^2 zm^2+(-2+\[Rho]2) \[Rho]2)^2 (-a^4 (zm^4-2 zm^2 \[Rho]2)-4 Sqrt[2] a \[Rho]2 Sqrt[-(-1+zm^2) \[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2)]+(-2+\[Rho]2) \[Rho]2^2 (2+\[Rho]2 (-1+2 \[Rho]2))+2 a^2 \[Rho]2 (\[Rho]2 (2+\[Rho]2)+zm^2 (2+(-5+\[Rho]2) \[Rho]2)))-\[Sqrt](1/\[Rho]2((-8 a^2 zm^2  (-2 a \[Rho]2 Sqrt[1-zm^2]+Sqrt[2] Sqrt[\[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2)])^2)/(a^2 zm^2+(-2+\[Rho]2) \[Rho]2)^2+(4 \[Rho]2^3 (a^4 (zm^2-zm^4)+2 (-2+\[Rho]2) \[Rho]2^2+a^2 \[Rho]2 (2+\[Rho]2-zm^2 \[Rho]2)-2 Sqrt[2] a Sqrt[-(-1+zm^2) \[Rho]2 (a^2+(-2+\[Rho]2) \[Rho]2) (a^2 zm^2+\[Rho]2^2)])^2)/(a^2 zm^2+(-2+\[Rho]2) \[Rho]2)^4)));


(* ::Subsection::Closed:: *)
(*Roots of the Polar equation*)


(*from Fujita& Hikida, Class.Quant.Grav.26 (2009) 135002*)


(* This code uses the polar equation (z^2-Subscript[z, -]^2)(a^2(1-T^2)z^2-Subscript[z, +]^2)\[Equal]0 as the Polar equation. Hence Subscript[z, +] is aSqrt[1-T^2]Subscript[z, +] in other sources. *)



(* ::Input::Initialization:: *)
KerrPolarRoot[a_,\[Rho]1_,\[Rho]2_,zm_,1]:= zm;


(* ::Input::Initialization:: *)
KerrPolarRoot[a_,\[Rho]1_,\[Rho]2_,zm_,2]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm]},
(a^2 \[Gamma][T]+L^2/(1-zm^2))^(1/2)
];


(* ::Input::Initialization:: *)
KerrPolarRoot[a_,\[Rho]1_,\[Rho]2_,1|1.|_Real?(N@#===1.&),2]:=KerrCarterConstant[a,\[Rho]1,\[Rho]2,1]^(1/2);


(* ::Subsection::Closed:: *)
(*Mino Radial Frequency*)


(*from Fujita& Hikida, Class.Quant.Grav.26 (2009) 135002*)


(* ::Input::Initialization:: *)
MinoRadialFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4)},
(\[Pi] Sqrt[\[Gamma][T](\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]4)])/(2elK[kr])
]
];


(* ::Input::Initialization:: *)
MinoRadialFrequency[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Infinity],\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Infinity],\[Rho]2,zm,4]},
With[{kr=(\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4)},
(\[Pi] Sqrt[2(\[Rho]2-\[Rho]4)])/(2elK[kr])
]
];


(* ::Input::Initialization:: *)
MinoRadialFrequency[0,r_,r_,zm_]:=Sqrt[((-6+r) r)/(-3+r)]


(* ::Subsection::Closed:: *)
(*Mino Polar Frequency*)


(*from Fujita& Hikida, Class.Quant.Grav.26 (2009) 135002*)


(* ::Input::Initialization:: *)
MinoPolarFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2],T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm]},
 (\[Pi]  zp)/(2elK[a^2 \[Gamma][T](zm/zp)^2])
];


(* ::Input::Initialization:: *)
MinoPolarFrequency[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=KerrPolarRoot[a,\[Infinity],\[Rho]2,zm,2];


(* ::Subsection::Closed:: *)
(*Mino Azimuthal Frequency*)


(*from Fujita& Hikida, Class.Quant.Grav.26 (2009) 135002*)


(* ::Input::Initialization:: *)
(*MinoAzimuthalFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2],T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm],\[Rho]in= 1-Sqrt[1-a^2],\[Rho]out:= 1+Sqrt[1-a^2]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)(\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),k\[Theta]=(zm/zp)^2,hout= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)(\[Rho]3-\[Rho]out)/(\[Rho]2-\[Rho]out),hin= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)(\[Rho]3-\[Rho]in)/(\[Rho]2-\[Rho]in)},
 ( L EllipticPi[zm^2,k\[Theta]])/elK[k\[Theta]]+a/(2Sqrt[1-a^2])((2 T \[Rho]out-a L)/(\[Rho]3-\[Rho]out)(1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]out)EllipticPi[hout,kr]/elK[kr])-(2 T \[Rho]in-a L)/(\[Rho]3-\[Rho]in)(1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]in)EllipticPi[hin,kr]/elK[kr]))
]
]*)


(* ::Input::Initialization:: *)
MinoAzimuthalFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoAzimuthalFrequencyr[a,\[Rho]1,\[Rho]2,zm]+MinoAzimuthalFrequency\[Theta][a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
MinoAzimuthalFrequency[0,r_,r_,0]:=r/Sqrt[-3+r]


(* ::Subsubsection:: *)
(*radial part*)


(* ::Input::Initialization:: *)
MinoAzimuthalFrequencyr[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm],\[Rho]in= 1-Sqrt[1-a^2],\[Rho]out:= 1+Sqrt[1-a^2]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),hout= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]out)/(\[Rho]2-\[Rho]out),hin= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]in)/(\[Rho]2-\[Rho]in)},
 a/(2Sqrt[1-a^2]) ((2 T \[Rho]out-a L)/(\[Rho]3-\[Rho]out) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]out) EllipticPi[hout,kr]/elK[kr])-(2 T \[Rho]in-a L)/(\[Rho]3-\[Rho]in) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]in) EllipticPi[hin,kr]/elK[kr]))
]
];


(* ::Input::Initialization:: *)
MinoAzimuthalFrequencyr[a:1|-1|_Real?(N@Abs@#===1.&),\[Rho]1_,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),hM= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-1)/(\[Rho]2-1)},
a T(2/(\[Rho]3-1) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-1) EllipticPi[hM,kr]/elK[kr])+
(2-a L/T)/(2(\[Rho]3-1)^2) ((2-((\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]3))/((\[Rho]1-1)(\[Rho]2-1)))+((\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]4)(\[Rho]3-1))/((\[Rho]1-1)(\[Rho]2-1)(\[Rho]4-1)) EllipticE[kr]/elK[kr]+(\[Rho]2-\[Rho]3)/(\[Rho]2-1) ((\[Rho]1-\[Rho]3)/(\[Rho]1-1)+(\[Rho]2-\[Rho]3)/(\[Rho]2-1)+(\[Rho]4-\[Rho]3)/(\[Rho]4-1)-4) EllipticPi[hM,kr]/elK[kr]))
]
];


(* ::Input::Initialization:: *)
MinoAzimuthalFrequencyr[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Infinity],\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Infinity],\[Rho]2,zm,4],L=KerrAngularMomentum[a,\[Infinity],\[Rho]2,zm],\[Rho]in= 1-Sqrt[1-a^2],\[Rho]out:= 1+Sqrt[1-a^2]},
With[{kr=(\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),hout= (\[Rho]3-\[Rho]out)/(\[Rho]2-\[Rho]out),hin= (\[Rho]3-\[Rho]in)/(\[Rho]2-\[Rho]in)},
 a/(2Sqrt[1-a^2]) ((2  \[Rho]out-a L)/(\[Rho]3-\[Rho]out) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]out) EllipticPi[hout,kr]/elK[kr])-(2  \[Rho]in-a L)/(\[Rho]3-\[Rho]in) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]in) EllipticPi[hin,kr]/elK[kr]))
]
];


(* ::Input::Initialization:: *)
MinoAzimuthalFrequencyr[a:1|-1|_Real?(N@Abs@#===1.&),Infinity|ComplexInfinity,\[Rho]2_,zm_]:=With[{\[Rho]3=KerrRadialRoot[a,\[Infinity],\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Infinity],\[Rho]2,zm,4],L=KerrAngularMomentum[a,\[Infinity],\[Rho]2,zm]},
With[{kr= (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),hM= (\[Rho]3-1)/(\[Rho]2-1)},
a(2/(\[Rho]3-1) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-1) EllipticPi[hM,kr]/elK[kr])+
(2-a L)/(2(\[Rho]3-1)^2) ((2-(\[Rho]2-\[Rho]3)/(\[Rho]2-1))+((\[Rho]2-\[Rho]4)(\[Rho]3-1))/((\[Rho]2-1)(\[Rho]4-1)) EllipticE[kr]/elK[kr]+(\[Rho]2-\[Rho]3)/(\[Rho]2-1) (1+(\[Rho]2-\[Rho]3)/(\[Rho]2-1)+(\[Rho]4-\[Rho]3)/(\[Rho]4-1)-4) EllipticPi[hM,kr]/elK[kr]))
]
];


(* ::Input::Initialization:: *)
MinoAzimuthalFrequencyr[0,r_,r_,0]:=0


(* ::Subsubsection:: *)
(*polar part*)


(* ::Input::Initialization:: *)

MinoAzimuthalFrequency\[Theta][a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2],T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm]},
 ( L EllipticPi[zm^2,a^2 \[Gamma][T](zm/zp)^2])/elK[a^2 \[Gamma][T](zm/zp)^2]
]


(* ::Input::Initialization:: *)
MinoAzimuthalFrequency\[Theta][a_,\[Rho]1_,\[Rho]2_,1|1.|_Real?(N@#===1.&)]:= \[Pi] Sqrt[((\[Rho]1 \[Rho]2 (a^4+\[Rho]1^2 \[Rho]2^2+a^2 ((-2+\[Rho]1) \[Rho]1+(-2+\[Rho]2) \[Rho]2))/2)/(a^4 (2+\[Rho]1+\[Rho]2)+\[Rho]1 \[Rho]2 (\[Rho]1^2 (-2+\[Rho]2)+\[Rho]1 (-2+\[Rho]2) \[Rho]2-2 \[Rho]2^2)+a^2 (\[Rho]1^3+\[Rho]1^2 \[Rho]2+\[Rho]1 (-4+\[Rho]2) \[Rho]2+\[Rho]2^3)))]/elK[(a^2 (a^4+\[Rho]1 (\[Rho]1 (-2+\[Rho]2)-2 \[Rho]2) \[Rho]2+a^2 (\[Rho]1^2+\[Rho]2^2)))/(\[Rho]1 \[Rho]2 (a^4+\[Rho]1^2 \[Rho]2^2+a^2 ((-2+\[Rho]1) \[Rho]1+(-2+\[Rho]2) \[Rho]2)))];


(* ::Input::Initialization:: *)
MinoAzimuthalFrequency\[Theta][a_,Infinity|ComplexInfinity,\[Rho]2_,1|1.|_Real?(N@#===1.&)]:= Sqrt[2] Sqrt[(\[Rho]2 (a^2+\[Rho]2^2))/(a^2+(-2+\[Rho]2) \[Rho]2)];


(* ::Subsection::Closed:: *)
(*Mino Gamma*)


(*from Fujita& Hikida, Class.Quant.Grav.26 (2009) 135002*)


(* ::Input::Initialization:: *)
(*MinoGamma[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm],Q=KerrCarterConstant[a,\[Rho]1,\[Rho]2,zm],\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2],\[Rho]in= 1-Sqrt[1-a^2],\[Rho]out= 1+Sqrt[1-a^2]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)(\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),k\[Theta]:=(zm/zp)^2,hout= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)(\[Rho]3-\[Rho]out)/(\[Rho]2-\[Rho]out),hin= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)(\[Rho]3-\[Rho]in)/(\[Rho]2-\[Rho]in),hr=(\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)},
(4 T+(T Q (1- EllipticE[k\[Theta]]/elK[k\[Theta]]))/(\[Gamma][T] zm^2)+T/2(\[Rho]3(\[Rho]1+\[Rho]2+\[Rho]3)-\[Rho]1 \[Rho]2+(\[Rho]1+\[Rho]2+\[Rho]3+\[Rho]4)(\[Rho]2-\[Rho]3)EllipticPi[hr,kr]/elK[kr]+(\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]4) EllipticE[kr]/elK[kr])+2T(\[Rho]3+(\[Rho]2-\[Rho]3)EllipticPi[hr,kr]/elK[kr])+2/(\[Rho]out-\[Rho]in)(((4T-a L)\[Rho]out-2a^2T)/(\[Rho]3-\[Rho]out)(1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]out) EllipticPi[hout,kr]/elK[kr])-((4T-a L)\[Rho]in-2a^2T)/(\[Rho]3-\[Rho]in)(1-( \[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]in)EllipticPi[hin,kr]/elK[kr])))
]
]
*)


(* ::Input::Initialization:: *)
MinoGamma[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoGammar[a,\[Rho]1,\[Rho]2,zm]+MinoGamma\[Theta][a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
MinoGamma[0,r_,r_,zm_]:=Sqrt[r^5/(-3+r)]


(* ::Subsubsection:: *)
(*radial part*)


(* ::Input::Initialization:: *)
MinoGammar[a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm],Q=KerrCarterConstant[a,\[Rho]1,\[Rho]2,zm],\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2],\[Rho]in= 1-Sqrt[1-a^2],\[Rho]out= 1+Sqrt[1-a^2]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),k\[Theta]:=(zm/zp)^2,hout= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]out)/(\[Rho]2-\[Rho]out),hin= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]in)/(\[Rho]2-\[Rho]in),hr=(\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)},
(a^2+4)T+T(1/2 (\[Rho]3(\[Rho]1+\[Rho]2+\[Rho]3)-\[Rho]1 \[Rho]2+(\[Rho]1+\[Rho]2+\[Rho]3+\[Rho]4)(\[Rho]2-\[Rho]3) EllipticPi[hr,kr]/elK[kr]+(\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]4) EllipticE[kr]/elK[kr])+2(\[Rho]3+(\[Rho]2-\[Rho]3) EllipticPi[hr,kr]/elK[kr])+1/Sqrt[1-a^2] (((4-a L/T)\[Rho]out-2a^2)/(\[Rho]3-\[Rho]out) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]out) EllipticPi[hout,kr]/elK[kr])-((4-a L/T)\[Rho]in-2a^2)/(\[Rho]3-\[Rho]in) (1-( \[Rho]2-\[Rho]3)/(\[Rho]2-\[Rho]in) EllipticPi[hin,kr]/elK[kr])))
]
];


(* ::Input::Initialization:: *)
MinoGammar[a:1|-1|_Real?(N@Abs@#===1.&),\[Rho]1_,\[Rho]2_,zm_]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],L=KerrAngularMomentum[a,\[Rho]1,\[Rho]2,zm],Q=KerrCarterConstant[a,\[Rho]1,\[Rho]2,zm],\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),k\[Theta]:=(zm/zp)^2,hM= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-1)/(\[Rho]2-1),hr=(\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)},
5T+T(1/2 ((\[Rho]3(\[Rho]1+\[Rho]2+\[Rho]3)-\[Rho]1 \[Rho]2)+(\[Rho]1+\[Rho]2+\[Rho]3+\[Rho]4)(\[Rho]2-\[Rho]3) EllipticPi[hr,kr]/elK[kr]+(\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]4)  EllipticE[kr]/elK[kr])+2(\[Rho]3+(\[Rho]2-\[Rho]3) EllipticPi[hr,kr]/elK[kr])+
(2(4-a L/T))/(\[Rho]3-1) (1-(\[Rho]2-\[Rho]3)/(\[Rho]2-1) EllipticPi[hM,kr]/elK[kr])+
(2-a L/T)/(\[Rho]3-1)^2 ((2-((\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]3))/((\[Rho]1-1)(\[Rho]2-1)))+((\[Rho]1-\[Rho]3)(\[Rho]2-\[Rho]4)(\[Rho]3-1))/((\[Rho]1-1)(\[Rho]2-1)(\[Rho]4-1)) EllipticE[kr]/elK[kr]+(\[Rho]2-\[Rho]3)/(\[Rho]2-1) ((\[Rho]1-\[Rho]3)/(\[Rho]1-1)+(\[Rho]2-\[Rho]3)/(\[Rho]2-1)+(\[Rho]4-\[Rho]3)/(\[Rho]4-1)-4) EllipticPi[hM,kr]/elK[kr]))
]
];


(* ::Input::Initialization:: *)
MinoGammar[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=\[Infinity];


(* ::Input::Initialization:: *)
MinoGammar[0,r_,r_,zm_]:=Sqrt[r^5/(-3+r)];


(* ::Subsubsection:: *)
(*polar part*)


(* ::Input::Initialization:: *)
MinoGamma\[Theta][a_,\[Rho]1_,\[Rho]2_,zm_]:=With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],Q=KerrCarterConstant[a,\[Rho]1,\[Rho]2,zm],zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2]},
(T Q)/(\[Gamma][T] zm^2) (1- EllipticE[a^2 \[Gamma][T](zm/zp)^2]/elK[a^2 \[Gamma][T](zm/zp)^2])-a^2 T
];


(* ::Input::Initialization:: *)
MinoGamma\[Theta][a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=With[{Q=KerrCarterConstant[a,\[Infinity],\[Rho]2,zm],zp=KerrPolarRoot[a,\[Infinity],\[Rho]2,zm,2]},
-a^2+(a^2 Q)/(2 zp^2)
];


(* ::Input::Initialization:: *)
MinoGamma\[Theta][a_,\[Rho]1_,\[Rho]2_,0]:=-a^2KerrEnergy[a,\[Rho]1,\[Rho]2,0]


(* ::Subsection:: *)
(*Mino Precession Frequencies*)


(* MvdM Unpublished *)


(* ::Input::Initialization:: *)
MinoPrecessionFrequency[a_,r1_,r2_,zm_]:=MinoPrecessionFrequencyr[a,r1,r2,zm]+MinoPrecessionFrequency\[Theta][a,r1,r2,zm]


(* ::Subsubsection:: *)
(*radial part*)


(* ::Input::Initialization:: *)
MinoPrecessionFrequencyr[a_,r1_,r2_,zm_]:=
With[
{
\[ScriptCapitalE]=KerrEnergy[a,r1,r2,zm],
\[ScriptCapitalL]=KerrAngularMomentum[a,r1,r2,zm],
\[ScriptCapitalQ]=KerrCarterConstant[a,r1,r2,zm],
r3=KerrRadialRoot[a,r1,r2,zm,3],
r4=KerrRadialRoot[a,r1,r2,zm,4],
\[CapitalUpsilon]r=MinoRadialFrequency[a,r1,r2,zm]
},
With[
{
\[ScriptCapitalK]=\[ScriptCapitalQ]+(a \[ScriptCapitalE]-\[ScriptCapitalL])^2,
kr= (r1-r2)/(r1-r3) (r3-r4)/(r2-r4)
},
(\[CapitalUpsilon]r Sqrt[\[ScriptCapitalK]])/\[Pi] ((2 (a^2 \[ScriptCapitalE]+r3^2 \[ScriptCapitalE]-a \[ScriptCapitalL]) EllipticK[kr])/(Sqrt[-(r1-r3) (r2-r4) (-1+\[ScriptCapitalE]^2)] (r3^2+\[ScriptCapitalK]))+( (r2-r3) (-a^2 \[ScriptCapitalE]+\[ScriptCapitalE] \[ScriptCapitalK]+a \[ScriptCapitalL]) (2Im[(r2+I Sqrt[\[ScriptCapitalK]]) (r3+I Sqrt[\[ScriptCapitalK]]) EllipticPi[((r1-r2) (r3-I Sqrt[\[ScriptCapitalK]]))/((r1-r3) (r2-I Sqrt[\[ScriptCapitalK]])),kr]]))/(Sqrt[(r1-r3) (r2-r4) (1-\[ScriptCapitalE]^2)] Sqrt[\[ScriptCapitalK]] (r2^2+\[ScriptCapitalK]) (r3^2+\[ScriptCapitalK])))
]
];


(* ::Subsubsection:: *)
(*polar part*)


(* ::Input::Initialization:: *)
MinoPrecessionFrequency\[Theta][a_,r1_,r2_,zm_]:=
With[
{
\[ScriptCapitalE]=KerrEnergy[a,r1,r2,zm],
\[ScriptCapitalL]=KerrAngularMomentum[a,r1,r2,zm],
\[ScriptCapitalQ]=KerrCarterConstant[a,r1,r2,zm],
zp=KerrPolarRoot[a,r1,r2,zm,2],
\[CapitalUpsilon]z=MinoPolarFrequency[a,r1,r2,zm]
},
With[
{
\[ScriptCapitalK]=\[ScriptCapitalQ]+(a \[ScriptCapitalE]-\[ScriptCapitalL])^2,
k\[Theta]= a^2 (1-\[ScriptCapitalE]^2)(zm/zp)^2
},
-((2\[CapitalUpsilon]z Sqrt[\[ScriptCapitalK]])/(\[Pi] zp))(\[ScriptCapitalE] EllipticK[k\[Theta]]+((a^2 \[ScriptCapitalE]-\[ScriptCapitalE] \[ScriptCapitalK]-a \[ScriptCapitalL])/\[ScriptCapitalK]) EllipticPi[(a^2 zm^2)/\[ScriptCapitalK],k\[Theta]] )
]
];


(* ::Subsection::Closed:: *)
(*Mino Frequencies*)


(* ::Input::Initialization:: *)
MinoFrequencies[a_,\[Rho]1_,\[Rho]2_,zm_]:= {MinoRadialFrequency[a,\[Rho]1,\[Rho]2,zm],MinoPolarFrequency[a,\[Rho]1,\[Rho]2,zm],MinoAzimuthalFrequency[a,\[Rho]1,\[Rho]2,zm]}


(* ::Subsection::Closed:: *)
(*Kerr Coordinate Frequencies (Killing time frequencies)*)


(* ::Input::Initialization:: *)
KerrRadialFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoRadialFrequency[a,\[Rho]1,\[Rho]2,zm]/MinoGamma[a,\[Rho]1,\[Rho]2,zm]
KerrPolarFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoPolarFrequency[a,\[Rho]1,\[Rho]2,zm]/MinoGamma[a,\[Rho]1,\[Rho]2,zm]
KerrAzimuthalFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoAzimuthalFrequency[a,\[Rho]1,\[Rho]2,zm]/MinoGamma[a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
KerrPrecessionFrequecy[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoPrecessionFrequency[a,\[Rho]1,\[Rho]2,zm]/MinoGamma[a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
KerrFrequencies[a_,\[Rho]1_,\[Rho]2_,zm_]:= {KerrRadialFrequency[a,\[Rho]1,\[Rho]2,zm],KerrPolarFrequency[a,\[Rho]1,\[Rho]2,zm],KerrAzimuthalFrequency[a,\[Rho]1,\[Rho]2,zm]}


(* ::Subsection:: *)
(*Proper time frequencies*)


(* ::Input::Initialization:: *)
ProperFrequencies[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoFrequencies[a,\[Rho]1,\[Rho]2,zm]/KerrProperFactor[a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
ProperRadialFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoRadialFrequency[a,\[Rho]1,\[Rho]2,zm]/KerrProperFactor[a,\[Rho]1,\[Rho]2,zm]
ProperPolarFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoPolarFrequency[a,\[Rho]1,\[Rho]2,zm]/KerrProperFactor[a,\[Rho]1,\[Rho]2,zm]
ProperAzimuthalFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoAzimuthalFrequency[a,\[Rho]1,\[Rho]2,zm]/KerrProperFactor[a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
ProperPrecessionFrequency[a_,\[Rho]1_,\[Rho]2_,zm_]:=MinoPrecessionFrequency[a,\[Rho]1,\[Rho]2,zm]/KerrProperFactor[a,\[Rho]1,\[Rho]2,zm]


(* ::Input::Initialization:: *)
(* ??? Where did this come from ??? Schmidt 2002???*)


(* ::Input::Initialization:: *)
KerrProperFactor[a_,\[Rho]1_,\[Rho]2_,zm_]:=
With[{T=KerrEnergy[a,\[Rho]1,\[Rho]2,zm],\[Rho]3=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,3],\[Rho]4=KerrRadialRoot[a,\[Rho]1,\[Rho]2,zm,4],zp=KerrPolarRoot[a,\[Rho]1,\[Rho]2,zm,2]},
With[{kr= (\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3) (\[Rho]3-\[Rho]4)/(\[Rho]2-\[Rho]4),k\[Theta]=a^2 (1-T^2)(zm/zp)^2,hr=(\[Rho]1-\[Rho]2)/(\[Rho]1-\[Rho]3)},
1/2 (-((2 zp^2)/(-1+T^2))+\[Rho]1 (-\[Rho]2+\[Rho]3)+\[Rho]3 (\[Rho]2+\[Rho]3))+((\[Rho]1-\[Rho]3) (\[Rho]2-\[Rho]4) EllipticE[kr])/(2 elK[kr])+(zp^2 EllipticE[k\[Theta]])/((-1+T^2) elK[k\[Theta]])+((\[Rho]2-\[Rho]3) (\[Rho]1+\[Rho]2+\[Rho]3+\[Rho]4) EllipticPi[hr,kr])/(2 elK[kr])
]]
KerrProperFactor[a_,\[Rho]1_,\[Rho]1_,_?PossibleZeroQ]:=\[Rho]1^2
KerrProperFactor[a_,Infinity|ComplexInfinity,\[Rho]2_,zm_]:=
\[Infinity]


(* ::Section:: *)
(*End*)


(* ::Input::Initialization:: *)
End[];


(* ::Input::Initialization:: *)
EndPackage[];
