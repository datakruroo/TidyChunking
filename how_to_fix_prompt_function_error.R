# 🔧 วิธีแก้ปัญหา: could not find function "prompt_function"
# ===========================================================

# ❌ สิ่งที่คุณทำ (ผิด):
my_prompt <- 'From the following text, please extract...'  # เป็น STRING

# ❌ นี่ทำให้เกิด error
word_test <- extract_competencies_tidyllm(
  chunks = test |> slice(1:5),
  max_per_chunk = 15,
  model = "gpt-4.1",           # ← model name ผิดด้วย
  custom_prompt = my_prompt    # ← ส่ง STRING แทน FUNCTION
)

# ✅ วิธีแก้ไข:

# 1. เปลี่ยน STRING เป็น FUNCTION:
my_prompt_function <- function(n_comp, hierarchy, text) {
  paste0(
    'From the following text, please extract "key terms" or "competency components" ',
    'relevant to undergraduate graduate teachers in elementary and early childhood education programs.\n\n',
    
    'Focus especially on competencies that align with these four main Program Learning Outcomes (PLOs):\n',
    '- PLO 1: Curriculum development, instructional design, teaching and learning innovation, technology integration, and data skills.\n',
    '- PLO 2: Adaptability and change management, professional ethics, lifelong learning, empathy, communication, teamwork, and leadership.\n',
    '- PLO 3: Growth mindset, lifelong learning, self-regulation, emotional intelligence, professional development, collaboration, and well-being.\n',
    '- PLO 4: Inclusive education, global citizenship, strategic/collaborative use of data, and engagement in professional learning communities.\n\n',
    
    'Additionally, please include any competencies that are specific to the field of elementary and early childhood education.\n\n',
    
    'For each competency, provide:\n',
    '- term: key word\n',
    '- plo: 1, 2, 3, 4, or "other"\n',
    '- category: knowledge, skill, behavior, technology, value, practice\n',
    '- brief_definition: short description\n',
    '- importance: high, medium, low\n\n',
    
    'Extract ', n_comp, ' competencies from section "', hierarchy, '":\n\n',
    text, '\n\n',
    'Return JSON format: [{"term": "name", "plo": "1", "category": "skill", "brief_definition": "desc", "importance": "high"}]'
  )
}

# 2. สร้าง custom schema:
plo_schema <- tidyllm::tidyllm_schema(
  name = "plo_competency_extraction",
  competencies = tidyllm::field_object(
    .description = "Array of PLO-aligned competencies",
    .vector = TRUE,
    term = tidyllm::field_chr(.description = "Competency term"),
    plo = tidyllm::field_fct(.levels = c("1", "2", "3", "4", "other")),
    category = tidyllm::field_fct(.levels = c("knowledge", "skill", "behavior", "technology", "value", "practice")),
    brief_definition = tidyllm::field_chr(.description = "Brief definition"),
    importance = tidyllm::field_fct(.levels = c("high", "medium", "low"))
  )
)

# 3. ใช้งานถูกต้อง:
word_test <- extract_competencies_tidyllm(
  chunks = test |> slice(1:5),
  max_per_chunk = 10,
  model = "gpt-4o-mini",                # ✅ ใช้ model name ที่ถูก
  custom_prompt = my_prompt_function,   # ✅ ส่ง FUNCTION
  custom_schema = plo_schema            # ✅ ใช้ custom schema
)

# ผลลัพธ์จะมีคอลัมน์:
# - term: ชื่อสมรรถนะ
# - plo: PLO ที่สอดคล้อง (1,2,3,4,other)
# - category: ประเภทสมรรถนะ
# - brief_definition: คำนิยาม
# - importance: ความสำคัญ
# - source_chunk: chunk ต้นทาง
# - source_hierarchy: hierarchy ต้นทาง

cat("✅ แก้ไขเรียบร้อย! ใช้ FUNCTION แทน STRING สำหรับ custom_prompt\n")
cat("✅ ใช้ model name ที่ถูกต้อง: gpt-4o-mini, gpt-4o, gpt-3.5-turbo\n") 
cat("✅ เพิ่ม custom_schema สำหรับ PLO structure\n")