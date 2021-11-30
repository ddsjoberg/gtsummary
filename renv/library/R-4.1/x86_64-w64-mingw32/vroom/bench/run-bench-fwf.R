#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source_file <- args[[1]]
out_file <- args[[2]]

file <- args[-c(1:2)]

library(vroom)

fields <- vroom(col_names = c("begin", "end", "width", "col_names"), delim = "\t",
"1	1	1	RECTYPE
2	8	7	SERIALNO
9	9	1	SAMPLE
10	11	2	STATE
12	12	1	REGION
13	13	1	DIVISION
14	18	5	PUMA5
19	23	5	PUMA1
24	27	4	MSACMSA5
28	31	4	MSAPMSA5
32	35	4	MSACMSA1
36	39	4	MSAPMSA1
40	41	2	AREATYP5
42	43	2	AREATYP1
44	57	14	TOTPUMA5
58	71	14	LNDPUMA5
72	85	14	TOTPUMA1
86	99	14	LNDPUMA1
100	101	2	SUBSAMPL
102	105	4	HWEIGHT
106	107	2	PERSONS
108	108	1	UNITTYPE
109	109	1	HSUB
110	110	1	HAUG
111	111	1	VACSTAT
112	112	1	VACSTATA
113	113	1	TENURE
114	114	1	TENUREA
115	116	2	BLDGSZ
117	117	1	BLDGSZA
118	118	1	YRBUILT
119	119	1	YRBUILTA
120	120	1	YRMOVED
121	121	1	YRMOVEDA
122	122	1	ROOMS
123	123	1	ROOMSA
124	124	1	BEDRMS
125	125	1	BEDRMSA
126	126	1	CPLUMB
127	127	1	CPLUMBA
128	128	1	CKITCH
129	129	1	CKITCHA
130	130	1	PHONE
131	131	1	PHONEA
132	132	1	FUEL
133	133	1	FUELA
134	134	1	VEHICL
135	135	1	VEHICLA
136	136	1	BUSINES
137	137	1	BUSINESA
138	138	1	ACRES
139	139	1	ACRESA
140	140	1	AGSALES
141	141	1	AGSALESA
142	145	4	ELEC
146	146	1	ELECA
147	150	4	GAS 
151	151	1	GASA
152	155	4	WATER 
156	156	1	WATERA
157	160	4	OIL 
161	161	1	OILA
162	165	4	RENT 
166	166	1	RENTA
167	167	1	MEALS
168	168	1	MEALSA
169	169	1	MORTG1
170	170	1	MORTG1A
171	175	5	MRT1AMT
176	176	1	MRT1AMTA
177	177	1	MORTG2
178	178	1	MORTG2A
179	183	5	MRT2AMT
184	184	1	MRT2AMTA
185	185	1	TAXINCL
186	186	1	TAXINCLA
187	188	2	TAXAMT
189	189	1	TAXAMTA
190	190	1	INSINCL
191	191	1	INSINCLA
192	195	4	INSAMT
196	196	1	INSAMTA
197	200	4	CONDFEE
201	201	1	CONDFEEA
202	203	2	VALUE
204	204	1	VALUEA
205	205	1	MHLOAN
206	206	1	MHLOANA
207	211	5	MHCOST
212	212	1	MHCOSTA
213	213	1	HHT
214	215	2	P65
216	217	2	P18
218	219	2	NPF
220	221	2	NOC
222	223	2	NRC
224	224	1	PSF
225	225	1	PAOC
226	226	1	PARC
227	227	1	SVAL
228	232	5	SMOC
233	235	3	SMOCAPI
236	236	1	SRNT
237	240	4	GRENT
241	243	3	GRAPI
244	244	1	FNF
245	245	1	HHL
246	246	1	LNGI
247	247	1	WIF
248	248	1	EMPSTAT
249	250	2	WORKEXP
251	258	8	HINC
259	266	8	FINC
")

fields$begin <- fields$begin - 1

types <- cols(
  .default = col_double(),
  RECTYPE = col_character(),
  SERIALNO = col_character(),
  STATE = col_character(),
  PUMA5 = col_character(),
  PUMA1 = col_character(),
  MSACMSA5 = col_character(),
  MSAPMSA5 = col_character(),
  MSACMSA1 = col_character(),
  MSAPMSA1 = col_character(),
  AREATYP5 = col_character(),
  AREATYP1 = col_character(),
  LNDPUMA5 = col_character(),
  TOTPUMA1 = col_character(),
  LNDPUMA1 = col_character(),
  SUBSAMPL = col_character(),
  HWEIGHT = col_character(),
  PERSONS = col_character(),
  BLDGSZ = col_character(),
  ELEC = col_character(),
  GAS = col_character(),
  WATER = col_character(),
  OIL = col_character(),
  RENT = col_character(),
  MRT1AMT = col_character(),
  MRT2AMT = col_character(),
  TAXAMT = col_character(),
  INSAMT = col_character(),
  CONDFEE = col_character(),
  VALUE = col_character(),
  MHCOST = col_character(),
  P65 = col_character(),
  P18 = col_character(),
  NPF = col_character(),
  NOC = col_character(),
  NRC = col_character(),
  SMOC = col_character(),
  SMOCAPI = col_character(),
  GRENT = col_character(),
  GRAPI = col_character(),
  WORKEXP = col_character(),
  HINC = col_character(),
  FINC = col_character()
)

cat(source_file, "\n")
out <- bench::workout_expressions(as.list(parse(source_file, keep.source = FALSE)))

x <- vroom::vroom(file, col_types = list())

out$size <- sum(file.size(file))
out$rows <- nrow(x)
out$cols <- ncol(x)
out$process <- as.numeric(out$process)
out$real <- as.numeric(out$real)
out$max_memory <- as.numeric(bench::bench_process_memory()[["max"]])

vroom::vroom_write(out, out_file)
